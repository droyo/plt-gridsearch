;;;; Graph data structure, w/ functions to generate various graphs. We
;;;; use a simple WxH adjacency matrix, where rows and columns
;;;; represent vertexes, and their intersections are true if there is
;;;; an edge between them. Note that this would be trivial to extend
;;;; to weighted edges by providing a number weight rather than #t
(module graph-create scheme
  (require srfi/1 srfi/43 "helper-functions.scm")

  ;; Graph creation
  ;; A graph with n vertices, all or none connected.
  (define (create-graph n weights)
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

  ;; Maze generation using Kruskal's algorithm. Change the shuffle 
  ;; to change the twistiness of the maze.
  (define (kruskal-maze graph loop-probability)
    (let ((possible-edges (edges graph))
          (connected? (lambda (v1 v2)
                        (member v2 (neighbors graph v1)))))
      ;; Remove all edges, put them back one by one
      (for-each (lambda (v)
                  (for-each (lambda (n)
                              (when (> (random) loop-probability)
                                (disconnect! graph v n)))
                            (neighbors graph v)))
                (vertices graph))
      (fold (lambda (edge forest)
              (let ((to (find (lambda (tree)
                                (member (first edge) tree))
                              forest))
                    (from (find (lambda (tree)
                                  (member (second edge) tree))
                                forest)))
                (cond 
                  ((and to from
                        (not (equal? to from)))
                   ;; The edge joins two trees, add it to the graph
                   (apply connect! graph edge)
                   (cons (append to from)
                               (remove (lambda (tree)
                                         (or (equal? tree to)
                                             (equal? tree from)))
                                       forest)))
                  (else forest))))
            (map list (vertices graph))
            (shuffle possible-edges))
      graph))
  
  ;;; Graph accessors. Use these rather than vector accessors, in case
  ;;; we want to change our data-structure in the future.
  (define graph-size
    vector-length)

  ;; Vertices are given integer names.
  (define (vertices graph)
    (list-tabulate (graph-size graph) values))

  ;; Return a list of edges ((from1 to1) (from2 to2) ...)
  ;; note we are using an undirected graph so (v1 v2) == (v2 v1)
  (define (edges graph)
    (define edge-list '())
 
    (define (add-edge from to)
      (set! edge-list
            (lset-union 
             (lambda (e1 e2)
               (or (equal? e1 e2)
                   (equal? e1 (reverse e2))))
             (list (list from to)) edge-list)))
    
      (vector-for-each
       (lambda (i row)
         (vector-for-each
          (lambda (j edge-exists?)
            (when edge-exists?
              (add-edge i j)))
          row))
         graph)
    edge-list)
                
            
  
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
	   vertices edges connect! disconnect! neighbors kruskal-maze))
