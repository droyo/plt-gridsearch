;;;; Notes on the GUI: I want it to be very spartan, ideally a huge
;;;; canvas area with buttons and a slider along the top and
;;;; bottom. Text output can be given in the repl we run the gui
;;;; from. This is not meant to be a standalone application.
(module graph-gui scheme/gui
  (require srfi/1 srfi/39
	   "graph.scm"
	   "graph-layout.scm"
	   "vector-operations.scm")

  ;; Some utility macros
  (define-syntax flip!
    (syntax-rules ()
      ((_ var)
       (begin (set! var (not var))
	      var))))

  ;; Add an item to the end of a list
  (define-syntax add!
    (syntax-rules ()
      ((_ item list-var)
       (set! list-var
	     (append list-var (list item))))))

  (define-syntax inc!
    (syntax-rules ()
      ((_ var x)
       (begin (set! var (+ var x))
	      var))
      ((_ var) (inc! var 1))))

  ;; Currently we draw nodes as black outlined circles with their names
  ;; in the middle.
  (define node-size (make-parameter 30))
  (define *padding* (node-size))
  (define edge-pen (make-parameter (send the-pen-list find-or-create-pen "BLACK" 1 'solid)))
  (define node-pen (make-parameter (send the-pen-list find-or-create-pen "BLACK" 1 'solid)))
  
  (define (draw-node dc name x y)
      (send dc set-pen (node-pen))
      (send dc draw-ellipse
	    (- x (/ (node-size) 2))
	    (- y (/ (node-size) 2))
	    (node-size)
	    (node-size))
      (send dc draw-text (number->string name)
	    ;; Assume a 8x4 font size
	    (- x 4)
	    (- y 8)))

  ;; Consider using paths to draw anti-aliased edges
  (define (draw-edge dc from to)
    (send dc set-pen (edge-pen))
    (send dc draw-line 
          (first from) (second from) 
          (first to) (second to)))

  ;; A unique list of edges
  (define (get-edges graph layout)
    (let* ((pos (lambda (x) (list-ref layout x)))
           (swap-endpoints reverse)
           (lines (apply append 
                         (map (lambda (vertex)
                                (map (lambda (adj)
                                       (list (pos vertex) (pos adj)))
                                     (neighbors graph vertex)))
                              (vertices graph)))))
      (delete-duplicates lines
                         (lambda (s1 s2)
                           (or (equal? s1 s2)
                               (equal? s1 (swap-endpoints s2)))))))

  (define (draw-graph canvas dc graph layout index)
    (let* ((width (send canvas get-width))
	   (height (send canvas get-height))
	   (size (list (- width (* 2 *padding*))
		       (- height (* 2 *padding*))))
	   (scale (lambda (pts)
		    (map (lambda (pt)
                           (v+ *padding* (v* size pt))) 
                         pts)))
	   (points (scale layout))
           (edges (map scale (get-edges graph layout))))

      (for-each (lambda (pos)
                  (apply draw-edge dc pos))
                edges)
      (for-each (lambda (name pos)
                  (apply draw-node dc name pos))
                (vertices graph) points)
      ;; Draw the graph number in lower left-hand corner
      (send dc draw-text (number->string index)
	    4 (- height 20))))

  (define layout (make-parameter random-layout))
  (define new-graph (make-parameter (lambda ()
				      (square-grid 3))))

  (define (display-graph)
    (define index 0)
    (define speed 1)
    (define running? #f)

    ;; using a struct rather than a list here might 
    ;; be easier to read
    (define (make-graph)
      (let ((g ((new-graph))))
	(list g ((layout)
		 (vertices g)
		 (lambda (v) (neighbors g v))))))
    (define graphs
      (list (make-graph)))

    
    ;; Keep our graphs in a list and add as we need them
    (define (to-graph x)
      (let ((new (+ index x)))
	(cond
	 ((>= new (length graphs))
	  (add! (make-graph) graphs)
	  (list-ref graphs (inc! index)))
	 ((>= new 0)
	  (list-ref graphs (inc! index x)))
	 (else
	  (list-ref graphs index)))))

    (define (current-graph)
      (to-graph 0))
  
    (define (draw canvas dc)
      (let ((graph (car (current-graph)))
	    (layout (cadr (current-graph))))
	(draw-graph canvas dc graph layout index)))

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

    (define (make-button name call)
      (new button%
	   [parent bar]
	   [label name]
	   [callback call]))

    ;; toggle start/stop
    (define (toggle-start button event)
      (if (flip! running?)
	  (send button set-label "Stop")
	  (send button set-label "Start")))

    (define start (make-button "Start" toggle-start))

    (define (update-speed button event)
      (set! speed (send button get-value)))
  
    (define slider
      (new slider%
	   [label ""]
	   [parent bar]
	   [min-value 1]
	   [max-value 1000]
	   [callback update-speed]))

    ;; If we're at graph 0 prev button is hidden
    (define (prev-graph button event)
      (to-graph -1)
      (send button show (> index 0))
      (send canvas refresh))
  
    (define prev (make-button "<" prev-graph))

    (define (next-graph button event)
      (to-graph +1)
      (send prev show (> index 0))
      (send canvas refresh))

    (define next (make-button ">" next-graph))

    ;; A closure that the user may use to control the window through
    ;; the repl. Does little to no error checking.
    (define (parse-cmd cmd . args)
      (let ((G (car (current-graph))))
	(case cmd
	  ((switch)
	   (and (number? (car args))
		(>= (car args) 0)
		(< (car args) (length graphs))
		(set! index (car args))
		(send canvas refresh)))
	  ((new-graph)
	   (parse-cmd 'switch (- (length graphs) 1))
	   (to-graph +1)
           (send prev show (> index 0))
           (printf "Created graph ~A~%" index))
	  ((connect)
	   (connect! G (car args) (cadr args))
           (send canvas refresh))
	  ((disconnect)
	   (disconnect! G (car args) (cadr args))
           (send canvas refresh))
          
	  (else
	   (printf "Unknown command ~A~%" cmd)))))

    (send prev show #f)
    (send win show #t)
    (sleep/yield 1)
    parse-cmd)

  (provide display-graph layout new-graph edge-pen node-pen node-size))