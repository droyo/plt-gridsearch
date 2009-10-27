(module graph-layout scheme
  (require srfi/1 srfi/43)
  ;;;; Force-based graph layout using the Fruchterman-Reingold
  ;;;; algorithm. We layout all the points on a unit plane that
  ;;;; stretches between 0 and 1, so we can scale it to any window size
  ;;;; by simply scaling the vectors
    
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
;; representation given the proper accessor functions.

  (define (random-layout nodes)
    (rand-points (length nodes)))

  (provide random-layout))