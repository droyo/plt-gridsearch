;;;; Helper functions and vector operations. They work any any size
;;;; vector, represented as lists (mathematical vectors, not the
;;;; scheme vector datatypes)
(module helper-functions scheme
  (require srfi/1)

  ;; utility functions
  (define (sum ls)
    (apply + ls))

  ;; Return first element of lst sorted by f.
  ;; (most > '(1 2 5 3 4)) -> 5
  (define (most f lst)
    (fold (lambda (x y)
            (if (f x y) x y))
          (car lst) lst))

  ;; #t If lst has no duplicates. Could be more efficient.
  (define (unique? lst)
    (= (length lst)
       (length (delete-duplicates lst))))
  

  ;; This shuffle takes an undefined amount of instructions.
  (define (shuffle lst)
    (let f ((order '()))
      (if (= (length order) (length lst))
	  (map (lambda (idx) (list-ref lst idx))
	       order)
	  (f (lset-union = (list (random (length lst)))
			 order)))))

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
  (define-syntax add!
    (syntax-rules ()
      ((_ item list-var)
       (begin (set! list-var
		    (append list-var (list item)))
	      list-var))))

  ;; Stack operations (combine pop! and add! to get a queue)
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
  ;; (let ((x 0)) (inc! x))   -> 1
  ;; (let ((x 3)) (inc! x 2)) -> 5
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

  (define (dist-squared v1 v2)
    (sum (map (compose square -) v2 v1)))

  (provide v/ v* v+ v- norm unit-vector dist-squared
	   unique? most square sum shuffle flip! add!
	   inc! push! pop!))