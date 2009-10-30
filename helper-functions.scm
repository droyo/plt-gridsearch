;;;; Helper functions and vector operations. They work any any size
;;;; vector, represented as lists (mathematical vectors, not the
;;;; scheme vector datatypes)
(module helper-functions scheme
  (require srfi/1)

  ;; utility functions
  (define (sum ls)
    (apply + ls))

  ;; #t If lst has no duplicate numbers
  (define (unique? lst)
    (= (length lst)
       (length (delete-duplicates lst))))
  
  ;; Knuth's Algorithm, from the Art of Computer Programming, Volume
  ;; 2, Section 3.4.2. Randomizes a list in O(n) time.
  (define (shuffle lst)
    (let ((v (list->vector lst)))
      (do ((n (length lst) (- n 1)))
	  ((zero? n) (vector->list v))
	(let* ((r (random n))
	       (t (vector-ref v r)))
	  (vector-set! v r (vector-ref v (- n 1)))
	  (vector-set! v (- n 1) t)))))

  (define (square x)
    (* x x))
  
  ;;; Some utility macros

  ;; flip a #t/#f variable like a switch, return new value
  (define-syntax flip!
    (syntax-rules ()
      ((_ var)
       (begin (set! var (not var))
	      var))))

  ;; Add an item to the end of a list
  ;; (define a '(1 2))
  ;; (add! 3 a) -> (1 2 3)
  ;; a -> (1 2 3)
  (define-syntax add!
    (syntax-rules ()
      ((_ item list-var)
       (begin (set! list-var
		    (append list-var (list item)))
	      list-var))))

  ;; Stack operations (combine pop! and add! to get a queue)
  ;; a -> (1 2 3)
  ;; (push! 0 a) -> (0 1 2 3)
  ;; a -> (0 1 2 3)
  ;; (pop! a) -> 0
  ;; a -> (1 2 3)
  (define-syntax push!
    (syntax-rules ()
      ((_ item list-var)
       (begin (set! list-var
		    (append list-var (list item)))
	      list-var))))

  ;; Returns a null list if the list is empty
  (define-syntax pop!
    (syntax-rules ()
      ((_ list-var)
       (let ((top (and (not (null? list-var))
		       (car list-var))))
	 (if top
	     (begin (set! list-var (cdr list-var))
		    top)
	     '())))))

  ;; Increment a numerical variable by x
  ;; (define x 6)
  ;; (inc! x)  -> 7
  ;; (inc! x 2) -> 9
  ;; x -> 9
  ;; (inc! x -4) -> 5
  ;; x -> 5
  (define-syntax inc!
    (syntax-rules ()
      ((_ var x)
       (begin (set! var (+ var x))
	      var))
      ((_ var) (inc! var 1))))

  ;; Create a function that takes any number of vector or scalar
  ;; arguments and combines them to form a new vector
  (define (vector-combination c init)
    (lambda args
      (fold (lambda (v1 v2)
              (cond ((and (list? v1) (list? v2))
                     (map c v1 v2))
                    ((list? v1)
                     (map c v1 (make-list (length v1) v2)))
                    ((list? v2)
                     (map c v2 (make-list (length v2) v1)))
                    (else
                     (c v1 v2))))
            init args)))

  ;; Vector arithmetic. These functions work on any size vectors
  (define v/ (vector-combination / 1))
  (define v* (vector-combination * 1))
  ;; Note: (v+ '(0 0) 1) -> '(1 1) is not a valid addition, but I'm not
  ;; gonna use it so I don't care. Think of it as shorthand.
  (define v+ (vector-combination + 0))
  (define v- (vector-combination - 0))
  
  (define (v-sum lst)
    (apply v+ lst))
  
  ;; I'm not sure of the terminology, but here the norm is defined as
  ;; the "length" or magnitude of the vector, written as |v|
  (define (norm v)
    (sqrt (sum (map square v))))

  ;; The unit vector of v has length 1, but retains the direction of v
  (define (unit-vector v)
    (v* v (/ 1 (norm v))))

  ;; Compute the distance without taking the square root
  (define (dist-squared v1 v2)
    (sum (map (compose square -) v2 v1)))

  (define dist
    (compose sqrt dist-squared))

  (provide v/ v* v+ v- norm unit-vector dist-squared dist
	   unique? square sum shuffle flip! add!
	   inc! push! pop!))