;;;; A Graph data structure, along with functions to generate
;;;; fully-connected graphs. The graphs used in this application will
;;;; be pretty dense, so for now we will be using a simple WxH
;;;; adjacency matrix, where rows and columns represent vertexes, and
;;;; their intersections have the value #f if there is no edge between
;;;; them, or a positive number indicating a weight.
(require srfi/1 srfi/43)

;; A graph with n vertices
(define (graph n fill)
  (list->vector
   (map (lambda (v)
	  (list->vector (list-tabulate n (lambda (x)
					   (if (= x v) #f fill)))))
	(list-tabulate n values))))

(define graph-size vector-length)

(define (vertices graph)
  (vector->list (vector-map (lambda (i w) i) graph)))

;; All vertices directly reachable from v
(define (neighbors graph v)
  (let ((connections (vector-map (lambda (idx val) (and val idx))
				 (vector-ref graph v))))
    (filter values (vector->list connections))))

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

;; This shuffle takes an undefined amount of instructions.
(define (shuffle lst)
  (let f ((order '()))
    (if (= (length order) (length lst))
	(map (lambda (idx) (list-ref lst idx))
	     order)
	(f (lset-union = (list (random (length lst)))
		       order)))))

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
