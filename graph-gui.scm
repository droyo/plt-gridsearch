;;;; Notes on the GUI: I want it to be very spartan, ideally a huge
;;;; canvas area with buttons and a slider along the top and
;;;; bottom. Text output can be given in the repl we run the gui
;;;; from. This is not meant to be a standalone application.
#lang scheme/gui

(require srfi/1
	 "graph.scm"
	 "graph-layout.scm"
	 "vector-operations.scm")

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
    ((_ var)
     (inc! var 1))))

(define *node-size* 30)
(define (draw-node dc name x y)
  (let ((stroke (make-object pen% "BLACK" 2 'solid)))
    (send dc set-pen stroke)
    (send dc draw-ellipse
	  (- x (/ *node-size* 2))
	  (- y (/ *node-size* 2))
	  *node-size* *node-size*)
    (send dc draw-text (number->string name)
	  (- x 4)
	  (- y 8))))

;; Only undirected edges for now.
(define (draw-edge dc from to)
  (let ((stroke (make-object pen% "GREY" 2 'solid)))
    (send dc draw-line (car from) (cadr from)
	  (car to) (cadr to))))

(define (draw-graph graph layout canvas dc)
  (let* ((size (list (- (send canvas get-width)
			*node-size*)
		     (- (send canvas get-height)
			*node-size*)))
	 (scale (lambda (pt) (v+ (/ *node-size* 2)
				 (v* size pt))))
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
	      (vertices graph) points)))

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
  (let ((g (random-graph 8 .3)))
    (list g (*layout-function* (vertices g)))))

;; Where the action's at
(define (run-grid-gui)
  (let* ((graphs (graph-list))
	 (current-graph 0)
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
				  c dc)))))
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
		      (send canvas refresh))))
	 (tempo 1)
	 (running #f))

    ;; [Start/Stop] [---|--------][<][>]
    (send win show #t)
    (sleep/yield 1)))
