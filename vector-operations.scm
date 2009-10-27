;;;; Vector operations. They work any any size vector, represented as
;;;; lists (mathematical vectors, not the scheme vector datatypes)
(module vector-operations scheme
  (require srfi/1)

  ;; utility functions
  (define (sum ls)
    (apply + ls))

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
  
  (define v/ (vector-combination / 1))
  (define v* (vector-combination * 1))
  ;; Note: (v+ '(0 0) 1) -> '(1 1) is not a valid addition, but I'm not
  ;; gonna use it so I don't care. Think of it as shorthand.
  (define v+ (vector-combination + 0))
  (define v- (vector-combination - 0))
  
  (define (v-sum lst)
    (apply v+ lst))
  
  (define (square x)
    (* x x))
  
  (define (norm v)
    (sqrt (sum (map square v))))

  (define (unit-vector v)
    (v* v (/ 1 (norm v))))

  (define (dist v1 v2)
    (sum (map (compose square -) v2 v1)))

  (provide v/ v* v+ v- norm unit-vector dist))