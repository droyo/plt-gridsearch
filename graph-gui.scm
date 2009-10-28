;;;; This is not meant to be a standalone application. You are supposed
;;;; to play with this from the repl.
(module graph-gui scheme/gui
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
  (define new-graph
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

  ;; Consider taking list of player trails and highlighting
  ;; edges with player colors.
  (define (draw-edge dc from to)
    (send dc set-pen (edge-pen))
    (send dc draw-line 
          (first from) (second from)
          (first to) (second to)))

  ;; For now players/goals are rounded rectangles
  (define (draw-player dc layout player)
    (let* ((pos (list-ref layout (first (player-trail player)))))
      (send dc set-pen (player-pen))
      (send dc draw-rounded-rectangle 
            (- (car pos) (/ (player-size) 2))
            (- (cadr pos)(/ (player-size) 2))
            (player-size) (player-size))))

  (define (draw-goal dc layout goal)
    (let* ((pos (list-ref layout goal)))
      (send dc set-pen (goal-pen))
      (send dc draw-rounded-rectangle
            (- (car pos) (/ (goal-size) 2))
            (- (cadr pos)(/ (goal-size) 2))
            (goal-size) (goal-size))))
  
  (define (draw-graph dc graph layout)
      (for-each (lambda (pos)
                  (apply draw-edge dc pos))
                (get-edges graph layout))
      (for-each (lambda (name pos)
                  (apply draw-node dc name pos))
                (vertices graph) layout))
  
  ;; A unique list of edges
  (define (get-edges graph layout)
    (let* ((pos (lambda (x) (list-ref layout x)))
           (swap-endpoints reverse)
           ;; The following generates list of lines
           ;; (((x1 y1) (x2 y2)) 
           ;;  ((x3 y3) (x4 y4)) ...)
           ;; For all vertices and their neighbors. 
           ;; It contains duplicates.
           (lines (apply append!
                         (map! (lambda (vertex)
                                 (map! (lambda (adj)
                                         (list (pos vertex) (pos adj)))
                                       (neighbors graph vertex)))
                               (vertices graph)))))
      (delete-duplicates
       lines
       ;; The lines ((x1 y1) (x2 y2)) and ((x2 y2) (x1 y1))
       ;; are considered equal.
       (lambda (s1 s2)
	 (or (equal? s1 s2)
	     (equal? s1 (swap-endpoints s2)))))))

  ;; Since we don't want players to pollute the graph, we need to
  ;; provide storage for visited nodes for each player. Player's 
  ;; current position will be at top of player-trail
  (define-struct player
    (name logic [trail #:mutable]))
  
  (define-struct graph 
    (;; The actual graph data structure as in graph.scm
     structure
     ;; The list of points for each node, mutable so we can
     ;; reset the layout
     [layout #:mutable]
     ;; We associate a list of players and goals with each graph
     [players #:mutable #:auto]
     [goals #:mutable #:auto])
    #:auto-value '())
  
  (define (create-graph-info)
    (let* ((g ((new-graph)))
           (l ((layout-function)
	       (vertices g) (lambda (v)
			      (neighbors g v)))))
      (make-graph g l)))

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
    
    ;;; Add/remove/manipulate players
    (define (add-player name fun 
                        [pos (random-start (graph-structure
					    (current-graph)))])
      (let ((new-player (make-player name fun (list pos))))
        (set-graph-players! (current-graph)
                            (cons new-player 
                                  (graph-players (current-graph)))))
      (send canvas refresh))
        
    (define (delete-player name)
      (set-graph-players! 
       (current-graph)
       (remove (lambda (p)
                 (string=? (player-name p) name))
               (graph-players (current-graph))))
      (send canvas refresh))

    ;; Moves the player by one graph node. The manner in which the
    ;; player moves about the graph is not set in stone yet, so this
    ;; area is subject to change.
    (define (step-player p)
      (let* ((g (graph-structure (current-graph)))
             (search (player-logic p))
             (pos (car (player-trail p)))
             (adj (neighbors g pos))
             (visited? (lambda (v) (member v (player-trail p))))
             (next (search adj visited?)))
        (if (member next adj)
            (set-player-trail! p (cons next (player-trail p)))
            (printf "~A is not reachable from ~A~%" next pos)))
      (send canvas refresh))

    ;; Graphs may have any number of goals.
    (define (add-goal vertex)
      (set-graph-goals! (current-graph)
                        (lset-union = ;avoid duplicate goals
				    (list vertex)
				    (graph-goals (current-graph))))
      (send canvas refresh))

    (define (delete-goal vertex)
      (set-graph-goals! (current-graph)
			(remove vertex (graph-goals (current-graph))))
      (send canvas refresh))

    ;; Our gui elements
    (define win
      (new frame%
	   [label "Graph Search"]
	   [min-width 320]
	   [min-height 200]))

    (define panel
      (new vertical-panel%
	   [parent win]
	   [alignment '(center top)]))

    ;; Currently the draw function redraws the entire graph, giving a
    ;; considerable flickering effect. An improvement would be only
    ;; redrawing the whole graph when the window is resized, instead
    ;; redrawing only the nodes that were tread on by players.
    (define (draw canvas dc)
      (let* ((graph (graph-structure (current-graph)))
             (size (list (- (send canvas get-width)
                            (* 2 (padding)))
                         (- (send canvas get-height)
                            (* 2 (padding)))))
             (layout (map (lambda (pt)
                            (v+ (v* size pt) (padding)))
                          (graph-layout (current-graph)))))

        (draw-graph dc graph layout)
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
      (move-prev-graph)
      ;; If we're at graph 0 prev button is hidden
      (send button show (> index 0))
      (when running?
        (toggle-start start/stop #t))
      (send canvas refresh))
    
    (define prev (make-button "<" prev-graph))

    (define (next-graph button event)
      (move-next-graph)
      ;; Make sure prev button is unhidden
      (send prev show (> index 0))
      (when running?
        (toggle-start start/stop #f))
      (send canvas refresh))

    (define next (make-button ">" next-graph))
    
    ;; A closure that the user may use to control the window through
    ;; the repl. I may just want to use 'eval' here, but it might be
    ;; better to keep things explicit.
    (define (parse-cmd cmd . args)
      (let ((G (graph-structure (current-graph))))
	(case cmd
	  ((jump)
	   (and (number? (car args))
		(>= (car args) 0)
		(< (car args) (length graph-list))
		(set! index (car args))
		(send canvas refresh)))
	  ;; Create a new graph and switch to it
	  ((new-graph)
	   (parse-cmd 'switch (- (length graph-list) 1))
	   (move-next-graph)
           (send prev show (> index 0))
           (printf "Created graph ~A~%" index))
	  ;; Modify the current graph
	  ((connect)
	   (apply connect! G args)
           (send canvas refresh))
	  ((disconnect)
	   (apply disconnect! G args)
           (send canvas refresh))
	  ;; Add/remove elements in the graph
          ((add-player)
           (apply add-player args))
          ((add-goal)
           (apply add-goal args))
          ((delete-player)
           (apply delete-player args))
          ((delete-goal)
           (apply delete-goal args))
	  ((toggle-start)
	   (toggle-start start/stop #t))
	  ;; Quit the session
          ((quit)
           (kill-thread main-thread)
	   (send win close))
	  (else
	   (printf "Unknown command ~A~%" cmd)))))
    
    ;; Turn on anti-aliasing
    (send (send canvas get-dc) set-smoothing 'aligned)
    (send prev show #f)
    (send win show #t)
    (sleep/yield 1);I don't know what this is for
    parse-cmd)

  (provide display-graph layout-function new-graph
	   player-pen goal-pen edge-pen node-pen
	   node-size player-size goal-size))