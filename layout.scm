;;;; Force-based graph layout using the Fruchterman-Reingold
;;;; algorithm. We layout all the points on a unit plane that
;;;; stretches between 0 and 1, so we can scale it to any window size
;;;; by simply scaling the vectors

;;; For force-directed layout we layout the points randomly and then
;;; iterate until we acheive a low-energy layout. Returning a vector
;;; because they're easier to modify, which we will be doing every
;;; iteration.
(define (rand-points n)
  (do ((points (lambda (_) (list (random) (random))))
       (l '(#t #t) (list-tabulate n points)))
      ((unique? l) (list->vector l))))

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

;;;; Vector operations. They work any any size vector, represented as
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
       (pos (rand-points (graph-size G) '(0 0)))
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