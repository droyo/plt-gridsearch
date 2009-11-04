;; Basing stuff on srfi-9 record types to keep things portable
(module graph-create scheme
  (require srfi/1 srfi/43 srfi/9 "helper-functions.scm")

  (define-record-type :graph
    (make-graph-record data size vertices)
    is-graph?
    (data     graph-data)
    (size     graph-size)
    (vertices graph-vertices)
    (edges    graph-edges    set-graph-edges!)
    (template graph-template set-graph-template!)
    (points   graph-points   set-graph-points!))

  (define (new-graph size edge-weight)
    (let* ((make-cols
	    (lambda (r)
	      (list->vector
	       (list-tabulate size
			      (lambda (c)
				(and (not (= c r))
				     edge-weight))))))
	   (data (list->vector (list-tabulate size make-cols)))
	   (size (vector-length data))
	   (vertices (list-tabulate size values))
	   (graph (make-graph-record data size vertices)))
      (init-graph graph)))

  ;; To avoid too much computation we store information about edges
  ;; and layout in the graph layout.
  (define (init-graph graph)
    (set-graph-edges! graph (list-edges (graph-data graph))))

  (define (list-edges graph-data)
    (let ((edges '())
	  (edge=? (lambda (e1 e2)
		    (or (equal? e1 e2)
			(equal? e1 (reverse! e2)))))
	  (add-edge! (lambda (e)
		       (set! edges (lset-union edge=?
					       (list e)
					       edges)))))
      (vector-for-each
       (lambda (row columns)
	 (vector-for-each
	  (lambda (col connected?)
	    (when connected?
		  (add-edge! (list row col))))
	  columns)
	 graph-data))
      edges))
  
  
  
  (provide new-graph))

