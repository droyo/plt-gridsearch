(module graph-layout scheme
  (require srfi/1 srfi/43 "vector-operations.scm")

  (define (most f lst)
    (fold (lambda (x y)
            (if (f x y) x y))
          (car lst) lst))
  
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

  ;; All of our layout functions use closures passed in as arguments to
  ;; manipulate graphs. This way, they will work for any graph
  ;; representation given the proper accessor function.
  (define (random-layout nodes get-neighbors)
    (rand-points (length nodes)))
  
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

  (provide random-layout grid-layout))