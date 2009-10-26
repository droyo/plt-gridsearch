(module graph-layout scheme
  (require srfi/1 srfi/43)
  ;;;; Force-based graph layout using the Fruchterman-Reingold
  ;;;; algorithm. We layout all the points on a unit plane that
  ;;;; stretches between 0 and 1, so we can scale it to any window size
  ;;;; by simply scaling the vectors
  
  ;; utility functions
  (define (sum ls)
    (apply + ls))
  
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
  
  ;;; For force-directed layout we layout the points randomly and then
  ;;; iterate until we acheive a low-energy layout.
  (define (rand-points n)
    (let ((p (list-tabulate n (lambda _ (list (random) (random))))))
      (if (unique? p) 
          p
          (rand-points n))))
  
  ;;; Vector operations. They work any any size vector, represented as
  ;;;; lists (mathematical vectors, not the scheme vector datatypes)
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
  
  (define v/ (vector-combination / 1 args))
  (define v* (vector-combination * 1 args))
  ;; Note: (v+ '(0 0) 1) -> '(1 1) is not a valid addition, but I'm not
  ;; gonna use it so I don't care. Think of it as shorthand.
  (define v+ (vector-combination + 0 args))
  (define v- (vector-combination - 0 args))
  
  (define (v-sum lst)
    (apply v+ lst))
  
  (define (square x)
    (* x x))
  
  (define (norm v)
    (sqrt (apply + (map square v)))))

(define (unit-vector v)
  (v* v (/ 1 (norm v))))

(define (dist v1 v2)
  (sum (map (compose square -) v2 v1)))

;; All of our layout functions use closures passed in as arguments to 
;; manipulate graphs. This way, they will work for any graph representation
;; given the proper accessor functions.
