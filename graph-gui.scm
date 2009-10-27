;;;; Notes on the GUI: I want it to be very spartan, ideally a huge
;;;; canvas area with buttons and a slider along the top and
;;;; bottom. Text output can be given in the repl we run the gui
;;;; from. This is not meant to be a standalone application.
#lang scheme/gui

(require srfi/1 "graph.scm" "graph-layout.scm" "vector-operations.scm")

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
  (let ((stroke (make-object pen% "BLACK" 2 'solid)))
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

(define (run-grid-gui)
  (let* ((graph (random-graph 6 .4))
	 (layout (random-layout (vertices graph)))
	 (win (new frame%
		   [label "Graph Search"]
		   [min-width 320]
		   [min-height 200]))
	 (panel (new vertical-panel%
		     [parent win]
		     [alignment '(center top)]))
	 (canvas (new canvas%
		      [parent panel]
		      [style '(border)]
		      [stretchable-width #t]
		      [stretchable-height #t]
		      [paint-callback
		       (lambda (canvas dc)
			 (draw-graph graph
				     layout
				     canvas
				     dc))]))
	 (bar (new horizontal-panel%
		   [parent panel]
		   [alignment '(center bottom)]
		   [stretchable-height #f]))
	 (make-button (lambda (name fun)
			(new button%
			     [parent bar]
			     [label name]
			     [callback fun])))
	 (tempo 1)
	 (running #f))

    (make-button "Start"
		 (lambda (b e)
		   (set! running (not running))
		   (if running
		       (send b set-label "Stop")
		       (send b set-label "Start"))))
    
    (new slider%
	 [label ""]
	 [parent bar]
	 [min-value 1]
	 [max-value 1000]
	 [callback (lambda (b e)
		     (set! tempo
			   (send b get-value))
		     (printf "Tempo: ~A~%" tempo))])

    (make-button "<" void)
    (make-button ">" void)

    (send win show #t)
    (sleep/yield 1)))
