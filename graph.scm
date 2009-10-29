;;;; Graph data structure, w/ functions to generate graphs. We use a
;;;; simple WxH adjacency matrix, where rows and columns represent
;;;; vertexes, and their intersections are true if there is an edge
;;;; between them. Note that this would be trivial to extend to
;;;; weighted edges by providing a number weight rather than #t
(module graph scheme
  (require srfi/1 srfi/43 "helper-functions.scm")

  ;; Graph creation
  ;; A graph with n vertices
  (define (create-graph n connected?)
    (list->vector
     (map (lambda (v)
	    (list->vector (list-tabulate
			   n (lambda (x)
			       (and (not (= x v))
				    connected?)))))
	  (list-tabulate n values))))

  ;; Randomly-connected graph
  (define (random-graph size prob)
    (do ((g (create-graph size #f))
	 (v1 0 (+ v1 1)))
	((>= v1 size) g)
      (for-each
       (lambda (v2)
	 (when (and (not (= v1 v2)) (< (random) prob))
	       (connect! g v1 v2)))
       (list-tabulate size values))))

  ;; Symmetrical graph, all vertices have max 4 neighbors.
  (define (square-grid n)
    (do ((g (create-graph (square n) #f))
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

  ;; Maze generation using depth-first-search
  (define (maze graph)
    (let* ((old '())
	   (visited? (lambda (v) (member v old)))
	   (expand (lambda (v)
		     (push! v old)
		     (shuffle (neighbors graph v))))
	   (start (random (graph-size graph))))

      (let dfs ((start start)
		(adj (expand start)))
	(let ((new (remove visited? adj)))
	  (cond
	   ;; When we hit a dead end, do nothing and pass execution
	   ;; back up the stack
	   ((null? new))
	   ;; The recursion has made a path to some adjacent nodes, so
	   ;; we can remove this extra path to them. Adding a
	   ;; probability here may make the graph more interesting.
	   ((any visited? adj)
	    (for-each (lambda (v)
			(disconnect! graph start v))
		      (filter visited? adj))
	    (dfs start new))

	   (else
	    (dfs (car new) (expand (car new)))
	    (dfs start (cdr new))))))
      graph))

  ;;; Graph accessors. Use these rather than vector accessors, in case
  ;;; we want to change our data-structure in the future.
  (define graph-size
    vector-length)

  ;; Vertices are given integer names.
  (define (vertices graph)
    (list-tabulate (graph-size graph) values))

  ;; List of vertices adjacent to v in graph
  (define (neighbors graph v)
    (let ((connections (vector-map (lambda (idx val) (and val idx))
				   (vector-ref graph v))))
      (filter values (vector->list connections))))

  ;; Modification (note: user search functions do not modify the
  ;; graph, though the user may do so through the repl)
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

  (provide create-graph graph-size square-grid random-graph
	   vertices connect! disconnect! neighbors maze))
