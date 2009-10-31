;;;; This is not meant to be a standalone application. You are supposed
;;;; to play with this from the repl.
(module graph-display scheme/gui
  (require srfi/1 srfi/39
	   "graph.scm"
	   "graph-layout.scm"
	   "helper-functions.scm")

  ;;; The following parameters are provided for the user to customize
  ;;; the program. You can set them by calling them like functions
  ;;; with their new value as an argument. Call them with no arguments
  ;;; to get their current value.
  ;;; (padding) -> 30
  ;;; (padding 23)
  ;;; (padding) -> 23

  ;; Margin between graph and edge of canvas (graph number is drawn here)
  (define padding (make-parameter 15))
  (define node-size (make-parameter 
                     30 (lambda (x);update padding when this is changed
			  (padding (/ x 2)) x)))

  (define player-size (make-parameter 20))
  
  ;; http://docs.plt-scheme.org/gui/pen-list_.html
  (define (pen color width)
    (send the-pen-list find-or-create-pen
	  color width 'solid))
  
  (define (brush color)
    (send the-brush-list find-or-create-brush
	  color 'solid))

  (define edge-pen (make-parameter (pen "Blue" 2)))
  (define node-pen (make-parameter (pen "Black" 1)))
  (define goal-pen (make-parameter (pen "Green" 1)))
  ;; color for node backgrounds
  (define background-brush (make-parameter (brush "SeaShell")))

  ;; True if we want player trails highlited
  (define highlight-player-trails (make-parameter #t))

  ;;; Drawing functions
  ;; draw-graph redraws the whole graph. only called on window refresh

  ;; Return a random color pen
  (define (random-pen width)
    (pen (random-choose '("Red" "RoyalBlue" "SteelBlue" "Dark Olive Green"
			  "DarkSeaGreen" "DarkKhaki" "Peru" "Sienna"
			  "Chocolate" "Orchid"))
	 width))

  (define (draw-graph dc graph-struct)
    (for-each (lambda (edge)
                (apply draw-edge dc (cdr edge)))
               (graph-edges graph-struct))
    (for-each (lambda (vertex pos)
                (apply draw-node dc
		       (number->string vertex) pos))
	      (vertices (graph-data graph-struct))
	      (graph-points graph-struct)))

  ;; Currently we draw nodes as black outlined circles with their names
  ;; in the middle.
  (define (draw-node dc name x y [pen (node-pen)])
      (send dc set-pen pen)
      (send dc draw-ellipse
	    (- x (/ (node-size) 2))
	    (- y (/ (node-size) 2))
	    (node-size) (node-size))
      (send dc draw-text name
	    ;; The name isn't centered properly
	    (- x (* 4 (remainder (string-length name) 6)))
	    (- y 8)))

  (define (draw-edge dc from to [pen (edge-pen)])
    (send dc set-pen pen)
    (send dc draw-line 
          (first from) (second from)
          (first to) (second to)))

  ;; Consider highlighting edges with player colors.
  ;; For now players/goals are rounded rectangles
  (define (draw-player dc graph-struct player)
    (let* ((trail (map (compose (lookup-positions graph-struct))
		       (player-trail player)))
	   (pos (first trail))
	   (breadcrumbs (lambda (a b)
			  (draw-edge dc a b
				     (player-pen player))
			  a)))
      (when (highlight-player-trails)
	    (fold breadcrumbs pos trail))

      (send dc set-pen (player-pen player))
      (send dc set-brush (brush (send (player-pen player) get-color)))

      (send dc draw-rounded-rectangle 
            (- (first pos) (/ (player-size) 2))
            (- (second pos)(/ (player-size) 2))
            (player-size) (player-size))

      (send dc set-brush (background-brush))))

  ;;; Here is the main program. We allocate all the resources
  ;;; for our gui, display the window, run the main loop and return
  ;;; a closure that the user can use to interact with the graph
  ;;; window
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
		   (compute-edge-positions! g))
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

  (provide display-graph node-pen goal-pen edge-pen
	   node-size player-size background-brush))
