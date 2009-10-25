#lang scheme

;;;; Graph data structure, w/ functions to generate graphs. We use a
;;;; simple WxH adjacency matrix, where rows and columns represent
;;;; vertexes, and their intersections are true if there is an edge
;;;; between them
(require srfi/1 srfi/43)

;;; Graph creation
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

;;;; Force-based graph layout using the Fruchterman-Reingold
;;;; algorithm. We layout all the points on a unit plane that
;;;; stretches between 0 and 1, so we can scale it to any window size
;;;; by simply scaling the vectors

;;; For force-directed layout we layout the points randomly and then
;;; iterate until we acheive a low-energy layout. Returning a vector
;;; because they're easier to modify, which we will be doing every
;;; iteration.
(define (rand-points n)
  (let ((p (list-tabulate n (lambda _ (list (random) (random))))))
    (if (unique? p) p (rand-points n))))

;;; Vector operations. They work any any size vector, represented as
;;;; lists (mathematical vectors, not the scheme vector datatypes)
(define (vector-combine c init args)
  (fold (lambda (v1 v2)
	  (cond ((and (list? v1) (list? v2))
		 (map c v1 v2))
		((list? v1)
		 (map c v1 (make-list (length v1) v2)))
		((list? v2)
		 (map c v2 (make-list (length v2) v1)))
		(else
		 (c v1 v2))))
	init args))

(define (v/ . args)
  (vector-combine / 1 args))

(define (v* . args)
  (vector-combine * 1 args))

;; Note: (v+ '(0 0) 1) -> '(1 1) is not a valid addition, but I'm not
;; gonna use it so I don't care. Think of it as shorthand.
(define (v+ . args)
  (vector-combine + 0 args))

(define (v- . args)
  (vector-combine - 0 args))

(define (mag v)
  (let ((sqr (lambda (x) (* x x))))
    (sqrt (apply + (map sqr v)))))

(define (norm v)
  (v* v (/ 1 (mag v))))

(define (dist v1 v2)
  (- (mag v2) (mag v1)))

;; Repulsion exerted between two nodes using coulomb's law
;; do we need the absolute value?
(define (node-force v1 v2)
  (let ((r (abs (dist v1 v2))))
    (v* v1 v2
	(/ 1 (* 4 pi))
	(norm (v- v1 v2))
	;; avoid division by 0
	(/ 1 (if (= r 0) .0001 r)))))

;; A vector of force exerted by an edge from v1 to v2. This is
;; incorrect. I need someone to figure it out for me!
(define (edge-force v1 v2 k)
  (v* -1 k (dist v1 v2)))

;;; The Fruchterman-Reingold algorithm. Adapted from the pseudocode on
;;; the wikipedia page.
(define (force-layout G threshold max-iterations k damping)
  (do ((nrg 0)
       (t 0 (+ t 1))
       (vert (vertices G))
       (vel (make-vector (graph-size G) '(0 0)))
       (pos (rand-points (graph-size G)))
       (energy (lambda ()
		 (inc! nrg
		       (apply + (map (lambda (v)
				       (v* v v))
				     (vector->list vel)))))))
      ((or (>= t max-iterations) (< (energy) threshold))
       (vector->list pos))

    (vector-for-each
     (lambda (vec p v)
       (let ((force 0)
	     (others (delete p (vector->list pos)))
	     (springs (neighbors G vec)))
	 (for-each (lambda (other-node)
		     (inc! force (node-force vec other-node)))
		   others)
	 (for-each (lambda (s)
		     (inc! force (edge-force vec s k)))
		   springs)
	 (vector-set! vel vec (v+ (vector-ref vel vec)
				  (* t force damping)))
	 (vector-set! pos vec (v+ (vector-ref pos vec)
				  (v* t (vector-ref vel vec))))))
     pos vel)))

;;; utility functions
(define (unique? lst)
  (= (length lst)
     (length (delete-duplicates lst))))

(define-syntax inc!
  (syntax-rules ()
    ((inc! var x)
     (begin (set! var (+ var x))
	    var))
    ((inc! var)
     (inc! var 1))))

;; This shuffle takes an undefined amount of instructions.
(define (shuffle lst)
  (let f ((order '()))
    (if (= (length order) (length lst))
	(map (lambda (idx) (list-ref lst idx))
	     order)
	(f (lset-union = (list (random (length lst)))
		       order)))))
