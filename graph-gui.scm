;;;; Notes on the GUI: I want it to be very spartan, ideally a huge
;;;; canvas area with buttons and a slider along the top and
;;;; bottom. Text output can be given in the repl we run the gui
;;;; from. This is not meant to be a standalone application.
#lang scheme/gui

(require srfi/1
	 "graph.scm"
	 "graph-layout.scm"
	 "vector-operations.scm")

;; Some utility macros
(define-syntax flip!
  (syntax-rules ()
    ((_ var)
     (begin (set! var (not var))
	    var))))

;; Add an item to the end of a list, return its index
(define-syntax add!
  (syntax-rules ()
    ((_ item list-var)
     (begin (set! list-var
		  (append list-var (list item)))
	    (length list-var)))))

(define-syntax inc!
  (syntax-rules ()
    ((_ var x)
     (begin (set! var (+ var x))
	    var))
    ((_ var) (inc! var 1))))

;; Currently we draw nodes as black outlined circles with their names
;; in the middle.
(define *node-size* 30)
(define *padding* *node-size*)

(define (draw-node dc name x y)
  (let ((stroke (make-object pen% "BLACK" 2 'solid)))
    (send dc set-pen stroke)
    (send dc draw-ellipse
	  (- x (/ *node-size* 2))
	  (- y (/ *node-size* 2))
	  *node-size* *node-size*)
    (send dc draw-text (number->string name)
	  ;; Assume a 8x4 font size
	  (- x 4)
	  (- y 8))))

;; Only undirected edges for now.
(define (draw-edge dc from to)
  (let ((stroke (make-object pen% "GRAY" 2 'solid)))
    (send dc set-pen stroke)
    (send dc draw-line
	  (car from) (cadr from)
	  (car to) (cadr to))))

(define (draw-graph graph layout index canvas dc)
  (let* ((width (send canvas get-width))
	 (height (send canvas get-height))
	 (size (list (- width (* 2 *padding*))
		     (- height (* 2 *padding*))))
	 (scale (lambda (pt)
		  (v+ *padding* (v* size pt))))
	 (points (map scale layout)))

    (for-each (lambda (node position)
		;; Draw the edges
		(for-each
		 (lambda (adj)
		   (draw-edge dc
			      position
			      (list-ref points adj)))
		 (neighbors graph node)))
	      (vertices graph) points)
    		;; Draw each node
    (for-each (lambda (node position)
		(apply draw-node dc node position))
	      (vertices graph) points)
    ;; Draw the graph number in lower left-hand corner
    (send dc draw-text (number->string index)
	  4 (- height 20))))

;; A canvas that grows to fill its container
(define (create-canvas par callback)
  (new canvas%
       [parent par]
       [style '(border)]
       [stretchable-width #t]
       [stretchable-height #t]
       [paint-callback callback]))

;; A closure that makes buttons for a container
(define (button-maker par)
  (lambda (name fun)
    (new button%
	 [parent par]
	 [label name]
	 [callback fun])))

;; A horizontal bar container
(define (create-bar par)
  (new horizontal-panel%
       [parent par]
       [alignment '(center bottom)]
       [stretchable-height #f]))

(define (create-window)
  (new frame%
       [label "Graph Search"]
       [min-width 320]
       [min-height 200]))

(define (create-panel par)
  (new vertical-panel%
       [parent par]
       [alignment '(center top)]))

(define (create-slider par fun)
  (new slider%
       [label ""]
       [parent par]
       [min-value 1]
       [max-value 1000]
       [callback fun]))

;; We'll manage a list of graphs that the user can cycle through using
;; the <> buttons
(define (graph-list)
  (list (new-graph)))

(define *layout-function* random-layout)
(define (new-graph)
  (let ((g (random-graph 5 1)))
    (list g (*layout-function* (vertices g)))))

;; Where the action's at
(define (run-grid-gui)
  (let* ((graphs (graph-list))
	 (current-graph 0)
	 (tempo 1)
	 (running #f)
	 
	 ;; Keep our graphs in a list and add as we need them
	 (to-graph
	  (lambda (x)
	    (cond
	     ((>= (+ current-graph x) (length graphs))
	      (add! (new-graph) graphs)
	      (list-ref graphs (inc! current-graph)))
	     ((> (length graphs) (+ current-graph x) -1)
	      (list-ref graphs (inc! current-graph x)))
	     (else
	      (list-ref graphs current-graph)))))

	 ;; Our gui elements
	 (win (create-window))
	 (panel (create-panel win))
	 (canvas (create-canvas
		  panel
		  (lambda (c dc)
		    (send dc clear)
		    (let ((g (to-graph 0)))
		      (draw-graph (car g) (cadr g)
				  current-graph c dc)))))
	 (bar (create-bar panel))
	 (make-button (button-maker bar))

	 (start (make-button
		 "Start" (lambda (b e)
			   (if (flip! running)
			       (send b set-label "Stop")
			       (send b set-label "Start")))))
	 (slider (create-slider
		  bar (lambda (b e)
			(set! tempo (send b get-value)))))

	 (prev (make-button
		"<" (lambda (b e)
		      (to-graph -1)
		      (send b show (> current-graph 0))
		      (send canvas refresh))))
	 (next (make-button
		">" (lambda (b e)
		      (to-graph +1)
		      (send prev show (> current-graph 0))
		      (send canvas refresh)))))

    ;; [Start/Stop] [---|--------][<][>]
    (send win show #t)
    (sleep/yield 1)))
