;;;; Here is the main program. We allocate all the resources
;;;; for our gui, display the window, run the main loop and return
;;;; a closure that the user can use to interact with the graph
;;;; window
(module graph-display scheme/gui
  (require srfi/1
	   "graph-create.scm"
	   "graph-layout.scm"
	   "graph-draw.scm"
	   "helper-functions.scm")

  (define (display-graph)
    (define pause 1)
    (define running? #f)
    (define graph-list (list (create-graph-struct)))
    (define index 0)
    (define window-width 400)
    (define window-height 420)

    ;; We move along the graph list relatively with the function
    ;; move-graph, which closes over the variables index and
    ;; graph-list. It adds new graphs when moving beyond the length of
    ;; our graph list.
    (define (move-graph x)
      (let ((new (+ index x)))
	(cond
	 ((>= new (length graph-list))
	  (add! (create-graph-struct) graph-list)
	  (check-scale-canvas #t)
	  (inc! index))
	 ((>= new 0)
	  (inc! index x))
         (else index))))

    ;; Some shortcuts to make things more readable
    (define (move-next-graph) (move-graph +1))
    (define (move-prev-graph) (move-graph -1))
    (define (current-graph)   (list-ref graph-list index))
    
    ;; Jump to a specific graph
    (define (jump-to-graph n)
      (when (> (length graph-list) n -1)
        (set! index n)
        (send canvas refresh)))

    (define (get-adjacents v)
      (neighbors (graph-data (current-graph)) v))

    ;; When called with no arguments, return a list of goals.  when
    ;; given arguments, returns #t if all arguments are goals
    (define (check-goal . args)
      (if (null? args)
	  (graph-goals (current-graph))
	  (every (lambda (v)
		   (member v (graph-goals (current-graph))))
		 args)))
    
    ;; Main step function that drives the player search.
    (define (step-player p)
      (let* ((now (first (player-trail p)))
             (visited? (lambda (v)
			 (member v (player-trail p))))
             (next (or (player-plan p)
		       ((player-search p) now get-adjacents visited? check-goal
			(lookup-positions (current-graph))))))
	;; If the search function returns a list, we follow the list,
	;; if it returns a single vertex, we follow that and call it
	;; again and again until it returns 'finished or 'no-path, or
	;; reaches the goal
	(cond
	 ((check-goal now)
	  (printf "~A Found goal in ~A steps~%"
		  (player-name p)
		  (length (player-trail p)))
	  (toggle-start start/stop #t)
	  (delete-player (current-graph) p)
	  (send canvas refresh))

	 ((null? (player-plan p))
	  (printf "~A Reached end of given path. ~%"
		  (player-name p))
	  ;; perhaps we should restart, here, calling the search
	  ;; function again?
	  (toggle-start start/stop #t)
	  (delete-player (current-graph) (player-name p))
	  (send canvas refresh))
	 
	 ((and (player-plan p)
	       (member (first (player-plan p))
		       (cons now (get-adjacents now))))
	  (draw-move-player p (pop-player-plan! p)))

	 ((list? next)
	  (printf "Received path ~A from ~A~%" next
		  (player-name p))
	  (set-player-plan! p next))
	 ;; Make sure player's aren't cheating
	 ((member next (get-adjacents now))
	  (draw-move-player p next))
	 ;; Return #f or 'no-path to give up
	 ((or (not next) (eq? next 'no-path))
	  (printf "~A Failed to find path to goal~%"
		  (player-name p))
	  (delete-player (current-graph) p))

	 (else
	  (printf "No cheating! There is no edge from ~A to ~A~%"
		  now next)
	  (delete-player (current-graph) p)))))
    
    ;; Our gui elements
    (define win
      (new frame%
	   [label "Graph Search"]
	   [min-width window-width]
	   [min-height window-height]))

    (define panel
      (new vertical-panel%
	   [parent win]
	   [alignment '(center top)]))

    (define (scale-pt p)
      (v+ (v* (list (- window-width (* 2 (padding)))
		    (- window-height (* 2 (padding))))
	      p)
	  (padding)))
    
    ;; Re-scale points when window is resized
    (define (check-scale-canvas [force #f])
      (let* ((width (send canvas get-width))
	     (height (send canvas get-height)))
	(unless (and (not force)
		     (= window-width width)
		     (= window-height height))
		(set! window-width width)
		(set! window-height height)
		(for-each
		 (lambda (g)
		   (set-graph-points! g (map scale-pt (graph-template g)))
		   (recompute-edge-positions g))
		 graph-list))))
    
    (define (draw canvas dc)
      (let ((g (current-graph)))
	(check-scale-canvas)
	(send dc set-brush (background-brush))
	(draw-graph dc g)

	(for-each (lambda (p)
		    (draw-player dc g p))
		  (graph-players g))

	(for-each (lambda (goal)
		    (let ((pt (list-ref (graph-points g) goal)))
		      (draw-node dc "G" (first pt) (second pt)
				 (goal-pen))))
		  (graph-goals g))
      ;; Draw graph number in lower left-hand corner
	(send dc draw-text (number->string index)
	      4 (- (send canvas get-height) 20))))

    (define (draw-move-player p next)
      ;; Redraw the node the player previously stepped on.
      (redraw-vertex (car (player-trail p)))
      (push-player-trail! p next)
      (draw-player (send canvas get-dc) (current-graph) p))

    (define (redraw-vertex v)
      (apply draw-node
	     (send canvas get-dc)
	     (number->string v)
	     (list-ref (graph-points (current-graph))
		       v)))

    (define canvas
      (new canvas%
	   [parent panel]
	   [style '(border)]
	   [stretchable-width #t]
	   [stretchable-height #t]
	   [paint-callback draw]))

    (define bar
      (new horizontal-panel%
	   [parent panel]
	   [alignment '(center bottom)]
	   [stretchable-height #f]))

    ;; Our buttons and slider
    (define (make-button name call)
      (new button%
	   [parent bar]
	   [label name]
	   [callback call]))

    ;; Our main loop just steps the players through the graph
    (define (main)
      (sleep/yield pause)
      (for-each step-player (graph-players (current-graph)))
      (main))
    ;; We run main in a separate thread, suspended and resumed by the
    ;; start button, so we can still play around in the repl.
    (define main-thread
      (let ((t (thread main)))
        (thread-suspend t) t))
    
    ;; toggle start/stop
    (define (toggle-start button event)
      (if (flip! running?)
          (begin (send button set-label "Stop")
                 (thread-resume main-thread))
          (begin (send button set-label "Start")
                 (thread-suspend main-thread))))
    
    (define start/stop
      (make-button "Start" toggle-start))
  
    (define (update-pause button event)
      (set! pause (/ (send button get-value) 50)))
    ;; Our slider controls length of pause between steps: higher is
    ;; slower.
    (define slider
      (new slider%
	   [label ""]
	   [parent bar]
	   [min-value 0]
	   [max-value 100]
           [init-value 50]
	   [callback update-pause]))

    ;; Our page buttons
    (define (prev-graph button event)
      (when running?
        (toggle-start start/stop #t))

      (move-prev-graph)
      ;; If we're at graph 0 prev button is hidden
      (send button show (> index 0))
      (send canvas refresh))
    
    (define prev (make-button "<" prev-graph))

    (define (next-graph button event)
      (when running?
        (toggle-start start/stop #f))

      (move-next-graph)
      ;; Make sure prev button is unhidden
      (send prev show (> index 0))
      (send canvas refresh))

    (define next (make-button ">" next-graph))
    
    ;; A closure that the user may use to control the window through
    ;; the repl. I may just want to use 'eval' here, but it might be
    ;; better to keep things explicit.
    (define (parse-cmd cmd . args)
      (case cmd
	((jump)
	 (apply jump-to-graph args)
	 (send prev show (> index 0)))
	;; Create a new graph and switch to it
	((new-graph)
	 (jump-to-graph (- (length graph-list) 1))
	 (next-graph next #t)
	 (printf "Created graph ~A~%" index))
	;; Modify the current graph
	((connect)
	 (apply add-edge (current-graph) args))
	((disconnect)
	 (apply remove-edge (current-graph) args))
	;; Add/remove elements in the graph
	((add-player)
	 (apply add-player (current-graph) (random-pen 2) args))
	((add-goal)
	 (apply add-goal (current-graph) args))
	;; Delete players by name, so some parsing is required
	((delete-player)
	 (map (lambda (name)
		(let ((player (find (lambda (p)
				      (string=? (player-name p)
						name))
				    (graph-players (current-graph)))))
		  (when player
			(apply delete-player (current-graph) player))))))
	((delete-goal)
	 (apply delete-goal (current-graph) args))
	((toggle-start)
	 (toggle-start start/stop #t))
	;; Quit the session
	((quit)
	 (kill-thread main-thread))
	(else
	 (printf "Unknown command ~A~%" cmd)))
      (send canvas refresh))
    
    ;; Turn on anti-aliasing
    (send (send canvas get-dc) set-smoothing 'aligned)
    (send prev show #f)
    (send win show #t)
    parse-cmd)

  (provide display-graph))
