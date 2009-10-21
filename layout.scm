;;;; Force-based graph layout using the Fruchterman-Reingold algorithm

(define (rand-points vertices width height)
  (do ((l '(#t #t) (map (lambda (v)
			  (list (random width)
				(random height)))
			vertices)))
      ((unique? l) l)))

(define (unique? lst)
  (not (any (lambda (l) (member l (rember l lst))) lst)))

(define (rember a lst)
  (cond ((null? lst) '())
	((equal? a (car lst))
	 (cdr lst))
	(else
	 (cons (car lst) (rember a (cdr lst))))))

;;;; Vector operations
(define (vector-combine c init . args)
  (fold (lambda (v1 v2)
	  (cond ((and (list? v1) (list? v2))
		 (list (c (car v1) (car v2))
		       (c (cadr v1) (cadr v2))))
		((list? v1)
		 (list (c (car v1) v2) (c (cadr v1) v2)))
		((list? v2)
		 (list (c (car v2) v1) (c (cadr v2) v1)))
		(else
		 (c v1 v2))))
	init args))

(define (v/ . args)
  (apply vector-combine / 1 args))

(define (v* . args)
  (apply vector-combine * 1 args))

;; Note: (v+ '(0 0) 1) -> '(1 1) is not a valid addition, but I'm not
;; gonna use it so I don't care
(define (v+ . args)
  (apply vector-combine + 0 args))

(define (v- . args)
  (apply vector-combine - 0 args))

(define (mag v)
  (let ((sqr (lambda (x) (* x x))))
    (sqrt (apply + (map sqr v)))))

(define (norm v)
  (v* v (/ 1 (norm v))))

(define (dist v1 v2)
  (- (norm v2) (norm v1)))

;; Repulsion exerted between two nodes using coulomb's law
;; do we need the abs?
(define (node-force v1 v2)
  (let ((r (abs (dist v1 v2))))
    (v* v1 v2
	(/ 1 (* 4 pi))
	(norm (v- v1 v2))
	;; avoid division by 0
	(/ 1 (if (= r 0) .0001 r)))))

;; A vector of force exerted by an edge between v1 and v2
(define (edge-force v1 v2)
  )