;; Basing stuff on srfi-9 record types rather than plt structs to keep
;; things portable
(module graph-create scheme
  (require srfi/1 srfi/43 srfi/9 "helper-functions.scm")

  ;; In our graphs, there can only be one edge between any two
  ;; vertices. Two edges are equal if they connect the same vertices.
  (define (edge=? e1 e2)
    (or (and (= (first e1) (first e2))
	     (= (second e1) (second e2)))
	(and (= (first e1) (second e2))
	     (= (second e1) (first e2)))))

  (define make-edge list)

  ;; Our graph data structure. Graph-data is an adjacency matrix, and
  ;; we keep various information about it, including information about
  ;; how it is drawn, to avoid recomputation.
  (define-record-type :graph
    (make-graph-record data size vertices)
    graph?
    (data     graph-data)
    (size     graph-size)
    (vertices graph-vertices)
    ;; vectors
    (template graph-template set-graph-template!)
    (points   graph-points   set-graph-points!))

  ;; Creates a fully or disconnected graph with size vertices
  (define (make-graph size edge-weight layout-function)
    (let* ((make-cols
	    (lambda (r)
	      (list->vector
	       (list-tabulate size
			      (lambda (c)
				(and (not (= c r))
				     edge-weight))))))
	   (data (list->vector
		  (list-tabulate size make-cols)))
	   (size (vector-length data))
	   (vertices (vector-unfold values size 0))
	   (graph (make-graph-record data size vertices)))
      (store-layout! graph layout-function)
      graph))

  (define (store-layout! graph layout-fn)
    (set-graph-template! graph (layout-fn graph))
    (set-graph-points! graph (vector-copy (graph-template graph)))
    ;; scale for a default window size of 300x300
    (scale-graph-points! graph 100 100))

  ;; The adjacency matrix is symmetric (this is an undirected graph),
  ;; we only have to count edges up to the diagonal.
  (define (graph-edges graph)
    (do ((row 0 (+ row 1))
	 (edges '()))
	((= row (graph-size graph)) edges)
      (do ((col 0 (+ col 1)))
	  ((> col row))
	(when (edge-weight graph row col)
	      (push! (make-edge row col)
		     edges)))))

  ;; When making new connections we have to update the edge list
  (define connect!
    (case-lambda
     ((graph v1 v2 weight)
      (vector-set! (vector-ref (graph-data graph) v1)
                   v2 weight)
      (vector-set! (vector-ref (graph-data graph) v2)
                   v1 weight))
     ((graph v1 v2) (connect! graph v1 v2 0))))

  (define (disconnect! graph v1 v2)
    (connect! graph v1 v2 #f))

  (define (edge-weight graph v1 v2)
    (vector-ref (vector-ref (graph-data graph) v1) v2))

  (define (adjacent? graph v1 v2)
    (if (edge-weight graph v1 v2) #t #f))

  (define (adjacents graph v)
    (let ((c (vector-map (lambda (idx v) (and v idx))
                         (vector-ref (graph-data graph) v))))
      (filter values (vector->list c))))
    
  ;;; Graphics specific features.
  ;; Scale the positions of our graphs on the screen if, for example,
  ;; the window is resized.
  (define (scale-graph-points! graph width height)
    (let ((window-size (list width height)))
      (vector-map! (lambda (i pt)
                     (v* pt window-size))
                   (graph-points graph))))

  (define (random-layout graph)
    (list-tabulate (graph-size graph)
		   (lambda (_)
		     (make-point (random) (random)))))

  (define (grid-layout graph)
    (let* ((size (round-up (sqrt (graph-size graph))))
           (scale (/ 1 size))
           (points (make-vector (graph-size graph)))
           (column -1))
      (vector-map! (lambda (v _)
                     (v+ (/ scale 2)
                         (v* (list (remainder (inc! column) size)
                                   (quotient column size))
                             scale)))
	   points)))
  
  ;;; Kruskal's algorithm creates an MST, useful for mazes
  ;; We'll use a disjoint forest to check for connections
  (define-record-type :node
    (make-node rank parent value)
    node?
    (rank node-rank set-rank!)
    (parent node-parent set-parent!)
    (value node-value))
  
  (define (make-set x)
    (set-parent! x x) x)
  
  (define (find-root x)
    (cond ((eq? (node-parent x) x)
           x)
          (else
           (set-parent! x (find-root (node-parent x)))
           (node-parent x))))
  
  (define (make-forest initial-vector)
    (vector-map (lambda (i v)
                  (make-set (make-node 0 #f v)))
                initial-vector))
  
  (define (merge-groups set x y)
    (let ((x-root (find-root (vector-ref set x)))
          (y-root (find-root (vector-ref set y))))
      (cond ((eq? x-root y-root)
             #f)
            ((> (node-rank x-root) (node-rank y-root))
             (set-parent! y-root x-root)
             (set-rank! x-root (+ (node-rank x-root) 1)))
            (else
             (set-parent! x-root y-root)
             (set-rank! y-root (+ (node-rank y-root) 1))))))
  
  ;; This algorithm is still too slow! Requires analysis -- DA
  (define (kruskal graph prob)
    (fold (lambda (edge forest)
            (unless (or (apply merge-groups forest edge)
                        (< (random) prob))
              (apply connect! graph edge))
            forest)
          (make-forest (graph-vertices graph))
          (sort (graph-edges graph)
                (lambda (e1 e2)
                  (< (apply edge-weight graph e1)
                     (apply edge-weight graph e2)))))
    graph)
  
  ;;; Functions to generate specific types of graphs and graph layouts
  (define (square-grid n)
    (let ((size (square n)))
      (do ((g (make-graph size #f grid-layout))
           (i 0 (+ i 1)))
        ((= i size) g)
        (when (>= (- i size) 0)
          (connect! g i (- i size)))
        (when (< (+ i size) size)
          (connect! g i (+ i size)))
        (when (> (remainder i size) 0)
          (connect! g i (- i 1)))
        (when (> (remainder (+ i 1) size) 0)
          (connect! g i (+ i 1))))))
  
  (define kruskal-maze
    (case-lambda
      ;; Adding a probability adds extra edges, i.e. multiple paths
      ((size prob)
       (kruskal (square-grid size) prob))
      ((size)
       (kruskal (square-grid size) 0))))

  (provide make-graph
           square-grid
           kruskal
           kruskal-maze
           
	   graph-size
	   graph-vertices
	   graph-edges
	   graph-template
	   graph-points   scale-graph-points!

	   random-layout
	   grid-layout

	   make-edge
	   edge-weight

	   connect!
	   disconnect!
	   adjacent?))

