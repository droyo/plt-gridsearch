;;;; Graph data structure, w/ functions to generate graphs. We use a
;;;; simple WxH adjacency matrix, where rows and columns represent
;;;; vertexes, and their intersections are true if there is an edge
;;;; between them
(module graph scheme
  (require srfi/1 srfi/43)

  ;; This shuffle takes an undefined amount of instructions.
  (define (shuffle lst)
    (let f ((order '()))
      (if (= (length order) (length lst))
	  (map (lambda (idx) (list-ref lst idx))
	       order)
	  (f (lset-union = (list (random (length lst)))
			 order)))))

  ;; Graph creation
  ;; A graph with n vertices
  (define (graph n connected?)
    (list->vector
     (map (lambda (v)
	    (list->vector (list-tabulate n (lambda (x)
					     (and (not (= x v)) connected?)))))
	  (list-tabulate n values))))

  ;; Randomly-connected graph
  (define (random-graph size prob)
    (do ((g (graph size #f))
	 (v1 0 (+ v1 1)))
	((>= v1 size) g)
      (for-each
       (lambda (v2)
	 (when (and (not (= v1 v2)) (< (random) prob))
	       (connect! g v1 v2)))
       (list-tabulate size values))))

  ;; Symmetrical graph, all vertices have max 4 neighbors.
  (define (square-grid n)
    (do ((g (graph (* n n) #f))
	 (i 0 (+ i 1)))
	((= i (* n n)) g)
      ;; Up
      (when (>= (- i n) 0)
	    (connect! g i (- i n)))
      ;; Down
      (when (< (+ i n) (* n n))
	    (connect! g i (+ i n)))
      ;; Left
      (when (positive? (remainder i n))
	    (connect! g i (- i 1)))
      ;; Right
      (when (positive? (remainder (+ i 1) n))
	    (connect! g i (+ i 1)))))


;;; Graph accessors
  (define graph-size
    vector-length)

  (define (vertices graph)
    (vector->list (vector-map (lambda (i w) i) graph)))

  ;; All vertices directly reachable from v
  (define (neighbors graph v)
    (let ((connections (vector-map (lambda (idx val) (and val idx))
				   (vector-ref graph v))))
      (filter values (vector->list connections))))

  (define (adjacent? graph v1 v2)
    (vector-ref (vector-ref graph v1) v2))

  ;; Modification (note: user search programs do not modify the graph)
  (define (modify! graph v1 v2 x)
    (cond ((not (< -1 v1 (graph-size graph)))
	   (error "The vertex does not exist" v1))
	  ((not (< -1 v2 (graph-size graph)))
	   (error "The vertex does not exist" v2))
	  (else
	   (vector-set! (vector-ref graph v1) v2 x)
	   (vector-set! (vector-ref graph v2) v1 x)
	   graph)))

  (define (connect! graph v1 v2)
    (modify! graph v1 v2 #t))

  (define (disconnect! graph v1 v2)
    (modify! graph v1 v2 #f))

  ;; A reference depth-first-search algorithm
  (define (search graph start end)
    (define visited '())

    (define (mark v)
      (set! visited (cons v visited)))

    (define (expand v)
      (mark v)
      (shuffle (lset-difference = (neighbors graph v) visited)))

    (let dfs ((now start)
	      (adj (expand start)))
      (cond ((member end adj)
	     (list now end))
	    ((null? adj) #f)
	    ((dfs (car adj) (expand (car adj)))
	     => (lambda (path)
		  (cons now path)))
	    (else
	     (dfs now (cdr adj))))))

  (provide graph graph-size square-grid random-graph
	   vertices connect! disconnect! search
	   neighbors adjacent?))
