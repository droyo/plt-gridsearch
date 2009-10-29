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
                     30 (lambda (x) ; update padding
			  (padding (/ x 2)) x)))

  ;; We want player and goal to be different sizes, so they don't block
  ;; each other if they are on the same vertex
  (define player-size (make-parameter 20))
  (define goal-size (make-parameter 18))
  
  (define edge-pen 
    (make-parameter (send the-pen-list find-or-create-pen 
                          "BLACK" 1 'solid)))
  (define node-pen 
    (make-parameter (send the-pen-list find-or-create-pen 
                          "BLACK" 1 'solid)))
  (define player-pen
    (make-parameter (send the-pen-list find-or-create-pen
                          "RED" 1 'solid)))
  (define goal-pen
    (make-parameter (send the-pen-list find-or-create-pen
                          "GREEN" 1 'solid)))
  ;; The layout function may be changed to any function that takes a list 
  ;; of vertices and a function to find adjacents and returns an equal-size 
  ;; list of points between (0, 0) and (1, 1)
  (define layout-function
    (make-parameter grid-layout))

  ;; The graph generation function takes no arguments and returns a graph 
  ;; object as described in graph.scm
  (define new-graph-function
    (make-parameter (lambda ()
		      (square-grid 3))))

  ;; Currently we draw nodes as black outlined circles with their names
  ;; in the middle.
  (define (draw-node dc name x y)
      (send dc set-pen (node-pen))
      (send dc draw-ellipse
	    (- x (/ (node-size) 2))
	    (- y (/ (node-size) 2))
	    (node-size)
	    (node-size))
      (send dc draw-text (number->string name)
	    ;; Assume a 8x4 font size. This doesn't work when name is
	    ;; more than one character long.
	    (- x 4)
	    (- y 8)))

  (define (draw-edge dc from to [pen (edge-pen)])
    (send dc set-pen pen)
    (send dc draw-line 
          (first from) (second from)
          (first to) (second to)))

  ;; Consider highlighting edges with player colors.
  ;; For now players/goals are rounded rectangles
  (define (draw-player dc layout player)
    (let* ((trail (map (lambda (x) (list-ref layout x))
		       (player-trail player)))
	   (cur (first trail))
	   (breadcrumbs
	    (lambda (b a)
	      (and a (draw-edge dc a b (player-pen)))
	      b)))
      (fold breadcrumbs #f trail)
      (send dc set-pen (player-pen))
      (send dc draw-rounded-rectangle 
            (- (first cur) (/ (player-size) 2))
            (- (second cur)(/ (player-size) 2))
            (player-size) (player-size))))

  (define (draw-goal dc layout goal)
    (let* ((pos (list-ref layout goal)))
      (send dc set-pen (goal-pen))
      (send dc draw-rounded-rectangle
            (- (first pos) (/ (goal-size) 2))
            (- (second pos)(/ (goal-size) 2))
            (goal-size) (goal-size))))
  
  (define (draw-graph dc vertices layout edge-points)
    (for-each (lambda (pos)
                (apply draw-edge dc pos))
              edge-points)
    (for-each (lambda (name pos)
                (apply draw-node dc name pos))
              vertices layout))
  
  ;; A unique list of edges
  (define (get-edges graph layout)
    (let* ((pos (lambda (x) (list-ref layout x)))
           (swap-endpoints reverse)
           ;; The following generates list of lines
           ;; (((v1 v2) (x1 y1) (x2 y2))
           ;;  ((v2 v1) (x2 y2) (x1 y1))
           ;;  ((v3 v4) (x3 y3) (x4 y4)) ...)
           ;; For all vertices and their neighbors. 
           ;; It contains duplicates.
           (lines (apply append!
                         (map! (lambda (vertex)
                                 (map! (lambda (adj)
                                         (list (list vertex adj)
                                               (pos vertex) 
                                               (pos adj)))
                                       (neighbors graph vertex)))
                               (vertices graph)))))
      (delete-duplicates
       lines
       ;; The lines ((x1 y1) (x2 y2)) and ((x2 y2) (x1 y1))
       ;; are considered equal.
       (lambda (s1 s2)
	 (or (equal? (car s1) (car s2))
	     (equal? (car s1) (swap-endpoints (car s2))))))))

  ;; Since we don't want players to pollute the graph, we need to
  ;; provide storage for visited nodes for each player. Player's 
  ;; current position will be at top of player-trail
  (define-struct player
    (name logic [trail #:mutable] [plan #:mutable #:auto])
    #:auto-value #f)
  
  (define-struct graph 
    (;; The actual graph data structure as in graph.scm
     structure
     ;; List of edge points so we don't recompute them each frame
     [edges #:mutable]
     ;; The list of points for each vertex. Our vertices are 
     ;; given integer names, so we find corresponding points in 
     ;; the layout using the vertex name as an index.
     [layout]
     ;; We associate a list of players and goals with each graph
     [players #:mutable #:auto]
     [goals #:mutable #:auto])
    #:auto-value '())
  
  (define (create-graph-info)
    (let* ((g ((new-graph-function)))
           (l ((layout-function)
	       (vertices g) (lambda (v)
			      (neighbors g v))))
           (e (get-edges g l)))
      (make-graph g e l)))

  ;;; Add/remove/manipulate players
  (define (add-player graph-info name fun [init #f])
    (let* ((start (or init (random-start (graph-structure graph-info))))
           (new-player (make-player name fun (list start))))
      (set-graph-players! graph-info (cons new-player 
                                           (graph-players graph-info)))))
  
  (define (delete-player graph-info name)
    (set-graph-players! graph-info (remove (lambda (p)
                                             (string=? (player-name p) name))
                                           (graph-players graph-info))))
  
  ;; Graphs may have any number of goals.
  (define (add-goal graph-info [init #f])
    (let ((pos (or init (random-start (graph-structure graph-info)))))
      (set-graph-goals! graph-info
                        (lset-union = ;avoid duplicate goals
                                    (list pos)
                                    (graph-goals graph-info)))))
  
  (define (delete-goal graph-info vertex)
    (set-graph-goals! graph-info
                      (remove vertex (graph-goals graph-info))))

  (define (add-edge graph-info from to)
    (let ((pos (lambda (x) (list-ref (graph-layout graph-info) x))))
      (set-graph-edges! graph-info
                        (cons (list (list from to)
                                    (pos from) (pos to))
                              (graph-edges graph-info)))
      (connect! (graph-structure graph-info) from to)))
  
  (define (remove-edge graph-info from to)
    (let ((pos (lambda (x) (list-ref (graph-layout graph-info) x)))
          (endpoints (list from to))
          (swap-endpoints reverse))
      (set-graph-edges! graph-info
                        (remove (lambda (e)
                                  (or (equal? (car e) endpoints)
                                      (equal? (swap-endpoints (car e)) 
                                              endpoints)))
                                (graph-edges graph-info)))
      (disconnect! (graph-structure graph-info) from to)))
  
  ;; Choose a random start. Note: we might want to make sure players
  ;; don't start at the same place
  (define (random-start graph)
    (let ((vt (vertices graph)))
      (list-ref vt (random (length vt)))))
  
  ;;; Here is the main program. We allocate all the resources
  ;;; for our gui, display the window, run the main loop and return
  ;;; a closure that the user can use to interact with the graph
  ;;; window
  (define (display-graph)
    (define pause 1)
    (define running? #f)
    (define graph-list (list (create-graph-info)))
    (define index 0)

    ;; We move along the graph list relatively with the function
    ;; move-graph, which closes over the variables index and
    ;; graph-list. It adds new graphs when moving beyond the length of
    ;; our graph list.
    (define (move-graph x)
      (let ((new (+ index x)))
	(cond
	 ((>= new (length graph-list))
	  (add! (create-graph-info) graph-list)
	  (inc! index))
	 ((>= new 0)
	  (inc! index x))
         (else index))))

    ;; Some shortcuts to make things more readable
    (define (move-next-graph) (move-graph +1))
    (define (move-prev-graph) (move-graph -1))
    (define (current-graph)   (list-ref graph-list index))
    
    ;; Jump to a specific graph
    (define (jump-to graph-index)
      (when (> (length graph-list) graph-index -1)
        (set! index graph-index)
        (send canvas refresh)))
    
    ;; Moves the player by one graph node. The manner in which the
    ;; player moves about the graph is not set in stone yet, so this
    ;; area is subject to change.
    (define (step-player p)

      (define (move-to next)
	(set-player-trail! p (cons next (player-trail p)))
	(apply draw-node (send canvas get-dc)
	       (second (player-trail p))
	       (scale-pt (list-ref (graph-layout (current-graph))
				   (second (player-trail p)))))
	(draw-player (send canvas get-dc)
		     (map scale-pt (graph-layout (current-graph)))
		     p))
      
      (let* ((g (graph-structure (current-graph)))
             (search (player-logic p))
             (now (first (player-trail p)))
             (adj (lambda (v) (neighbors g v)))
             (visited? (lambda (v) (member v (player-trail p))))
	     (goal? (lambda (v) (member v (graph-goals (current-graph)))))
             (next (or (player-plan p)
		       (search now adj visited? goal?))))
	;; If the search function returns a list, we follow the list,
	;; if it returns a vertice, we follow that and call it again
	;; and again until it returns 'finished or 'no-path
	(cond
	 ((goal? now)
	  (printf "~A Found goal in ~A steps~%"
		  (player-name p)
		  (length (player-trail p)))
	  (toggle-start start/stop #t)
	  (delete-player (current-graph) (player-name p))
	  (send canvas refresh))

	 ((null? (player-plan p))
	  (printf "~A Reached end of given path. ~%"
		  (player-name p))
	  (toggle-start start/stop #t)
	  (delete-player (current-graph) (player-name p))
	  (send canvas refresh))
	 
	 ((and (player-plan p)
	       (member (first (player-plan p)) (cons now (adj now))))
	  (move-to (first (player-plan p)))
	  (set-player-plan! p (cdr (player-plan p))))

	 ((list? next)
	  (printf "Received path ~A from ~A~%" next
		  (player-name p))
	  (set-player-plan! p next))
	 ;; Make sure player's aren't cheating
	 ((member next (adj now))
	  (move-to next))
	 ;; Return #f or 'no-path to give up
	 ((or (not next) (eq? next 'no-path))
	  (printf "~A Failed to find path to goal~%"
		  (player-name p))
	  (delete-player (current-graph) (player-name p)))

	 (else
	  (printf "No cheating! There is no edge from ~A to ~A~%"
		  now next)
	  (delete-player (current-graph) (player-name p))))))
    
    ;; Our gui elements
    (define win
      (new frame%
	   [label "Graph Search"]
	   [min-width 640]
	   [min-height 480]))

    (define panel
      (new vertical-panel%
	   [parent win]
	   [alignment '(center top)]))

    ;; Currently the draw function redraws the entire graph, giving a
    ;; considerable flickering effect. An improvement would be only
    ;; redrawing the whole graph when the window is resized, instead
    ;; redrawing only the nodes that were tread on by players. This
    ;; flickering gets really bad as the graph gets larger.
    (define (scale-pt p)
      (v+ (v* (list (- (send canvas get-width)
		       (* 2 (padding)))
		    (- (send canvas get-height)
		       (* 2 (padding))))
	      p)
	  (padding)))

    (define (draw canvas dc)
      (let* ((vert (vertices (graph-structure (current-graph))))
             (scale (lambda (pts) (map scale-pt pts)))
             (layout (scale (graph-layout (current-graph))))
             (edges (map (compose scale cdr)
                         (graph-edges (current-graph)))))

        (draw-graph dc vert layout edges)
        (for-each (lambda (p)
                    (draw-player dc layout p))
                  (graph-players (current-graph)))
        (for-each (lambda (g)
                    (draw-goal dc layout g))
                  (graph-goals (current-graph)))
        ;; Draw graph number in lower left-hand corner
        (send dc draw-text (number->string index)
              4 (- (send canvas get-height) 20))))

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
      (sleep pause)
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
      (let ((G (graph-structure (current-graph))))
	(case cmd
	  ((jump)
           (apply jump-to args)
           (send prev show (> index 0)))
	  ;; Create a new graph and switch to it
	  ((new-graph)
	   (jump-to (- (length graph-list) 1))
	   (move-next-graph)
           (send prev show (> index 0))
           (printf "Created graph ~A~%" index))
	  ;; Modify the current graph
	  ((connect)
           (apply add-edge (current-graph) args))
	  ((disconnect)
           (apply remove-edge (current-graph) args))
	  ;; Add/remove elements in the graph
          ((add-player)
           (apply add-player (current-graph) args))
          ((add-goal)
           (apply add-goal (current-graph) args))
          ((delete-player)
           (apply delete-player (current-graph) args))
          ((delete-goal)
           (apply delete-goal (current-graph) args))
	  ((toggle-start)
	   (toggle-start start/stop #t))
	  ;; Quit the session
          ((quit)
           (kill-thread main-thread)
	   (send win close))
	  (else
	   (printf "Unknown command ~A~%" cmd))))
      (send canvas refresh))
    
    ;; Turn on anti-aliasing
    (send (send canvas get-dc) set-smoothing 'aligned)
    (send prev show #f)
    (send win show #t)
    (sleep/yield 1);I don't know what this is for
    parse-cmd)

  (provide display-graph layout-function new-graph-function
	   node-size player-size goal-size))