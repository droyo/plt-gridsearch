(module graph-layout scheme
  (require srfi/1 srfi/43 "helper-functions.scm" "graph.scm")

    ;;; Our graph and player data structures: While the adjacency matrix
  ;;; in graph.scm is fine for checking connections between vertices,
  ;;; we need more information, such as the position of vertices on
  ;;; the screen, and objects on the vertices.
  (define-struct graph 
    (data 		;The actual graph data structure as in graph.scm
     template           ;Original list of points from layout functions
     [points #:mutable] ;The list of points for each vertex, scaled to
			;window size from template. 
     [edges #:mutable #:auto]	;List of edge points so we don't recompute them each frame
     [players #:mutable #:auto]
     [goals #:mutable #:auto])
    #:auto-value '())
  
  (define (create-graph-struct)
    (let* ((data ((new-graph-function)))
	   (layout (layout-function))
           (points (layout (vertices data)
			   (lambda (v)
			     (neighbors data v))))
	   (struct (make-graph data points points)))
      (compute-edge-positions! struct)
      struct))
  
  ;; Since we don't want players to pollute the graph, we need to
  ;; provide storage for visited nodes for each player. The player's
  ;; current position will be at top of player-trail
  (define-struct player
    (name search [trail #:mutable] pen [plan #:mutable #:auto])
    #:auto-value #f)

  ;; Make it a little easier to modify struct variables
  (define (struct-pop get set top rest)
    (lambda (s)
      (and (not (null? (get s)))
	   (let ((t (top (get s))))
	     (set s (rest (get s)))
	     t))))

  (define (struct-push get set combine)
    (lambda (s val)
      (set s (combine val (get s)))
      (get s)))

  (define push-player-plan!
    (struct-push player-plan set-player-plan! cons))

  (define pop-player-plan!
    (struct-pop  player-plan set-player-plan! first cdr))

  (define push-player-trail!
    (struct-push player-trail set-player-trail! cons))

  (define push-graph-player!
    (struct-push graph-players set-graph-players! cons))

  (define push-graph-goal!
    (struct-push graph-goals set-graph-goals! cons))

  (define push-graph-edge!
    (struct-push graph-edges set-graph-edges! cons))

  (define (lookup-positions graph-struct)
    (lambda (v)
      (list-ref (graph-points graph-struct) v)))

  ;;; Add/remove/manipulate players
  (define (add-player graph-struct pen name search [init #f])
    (let ((start (list (or init (random-start graph-struct)))))
      (push-graph-player! graph-struct
			  (make-player name search start pen))))
  
  (define (delete-player graph-struct p)
    (set-graph-players! graph-struct
			(delete p (graph-players graph-struct))))

  ;; Graphs may have any number of goals.
  (define (add-goal graph-struct [init #f])
    (push-graph-goal! graph-struct
		      (or init (random-start graph-struct))))
  
  (define (delete-goal graph-struct vertex)
    (set-graph-goals! graph-struct
                      (delete vertex (graph-goals graph-struct))))

  (define (add-edge graph-struct from to)
    (let ((pos (lookup-positions graph-struct)))
      (push-graph-edge! graph-struct
			(list (list from to)
			      (pos from) (pos to)))
      (connect! (graph-data graph-struct) from to)
      (compute-edge-positions! graph-struct)))
  
  (define (remove-edge graph-struct from to)
    (let ((pos (lookup-positions graph-struct))
          (endpoints (list from to)))
      (set-graph-edges! graph-struct
                        (remove (lambda (e)
                                  (or (equal? endpoints
					      (car e))
                                      (equal? endpoints 
                                              (reverse (car e)))))
                                (graph-edges graph-struct)))
      (disconnect! (graph-data graph-struct) from to)))
  
  ;; A unique list of edges
  (define (compute-edge-positions! graph-struct)
    (let* ((pos (lookup-positions graph-struct)))
      ;; The following generates list of lines
      ;; (((v1 v2) (x1 y1) (x2 y2))
      ;;  ((v2 v1) (x2 y2) (x1 y1))
      ;;  ((v3 v4) (x3 y3) (x4 y4)) ...)
      ;; For all vertices and their neighbors. 
      ;; It does not contain duplicates.
      (set-graph-edges! graph-struct
			(map (lambda (line)
			       (let ((from (pos (first line)))
				     (to (pos (second line))))
				 (list line from to)))
			     (edges (graph-data graph-struct))))))

  ;; Choose a random start. Note: we might want to make sure players
  ;; don't start at the same place
  (define (random-start graph-struct)
    (random-choose (vertices (graph-data graph-struct))))


  ;;; Layout the points randomly
  (define (rand-points n)
    (let ((p (list-tabulate n (lambda _ (list (random) (random))))))
      (if (unique? p) 
          p
          (rand-points n))))

  ;; All of our layout functions use closures passed in as arguments to
  ;; manipulate graphs. This way, they will work for any graph
  ;; representation given the proper accessor function.
  (define (random-layout nodes get-neighbors)
    (rand-points (length nodes)))
  
  ;; We assume that the number of nodes is a perfect square and the
  ;; grid is generated by the square-grid function. Otherwise results
  ;; may vary
  (define (grid-layout nodes get-neighbors)
    (let* ((size (sqrt (length nodes)))
           (scale (/ 1 size))
           (column -1))
      (map (lambda (node)
             (v+ (/ scale 2)
                 (v* (list (remainder (inc! column) size)
                           (quotient column size))
                     scale)))
             nodes)))

  (define layout-function
    (make-parameter grid-layout))

  (provide layout-function random-layout grid-layout
	   create-graph-struct push-player-plan! pop-player-plan!
	   lookup-positions add-player delete-player
	   add-edge remove-edge compute-edge-positions!
	   random-start graph-edges graph-data graph-points
	   player-trail player-plan player-name player-pen
	   graph-goals player-search
	   set-player-plan! set-graph-points! graph-template
	   graph-players push-player-trail! add-goal delete-goal))
