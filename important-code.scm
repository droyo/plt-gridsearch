;;; Code that I want to save

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
                   ;; The edge joins two trees, add it to the graph.
		   ;; this is a very inefficient operation and needs
		   ;; to be replaced in the future
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

  ;; Symmetrical graph, all vertices have max 4 neighbors.


  ;; Maze generation using Kruskal's algorithm. Change the shuffle 
  ;; to change the twistiness of the maze.

  
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