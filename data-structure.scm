;;;; A Graph data structure, along with functions to generate
;;;; fully-connected graphs. The graphs used in this application will
;;;; be pretty dense, so for now we will be using a simple WxH
;;;; adjacency matrix, where rows and columns represent vertexes, and
;;;; their intersections have the value #f if there is no edge between
;;;; them, or a positive number indicating a weight.
(use srfi-1 srfi-43)

(define (2d-vector width height fill)
  (make-vector height
	       (make-vector width fill)))

(define (vector-map fn vec)
  (let ((copy ))))

(define (disconnected-graph n)
  (2d-vector n n #f))

(define (fully-connected-graph n)
  (2d-vector n n 0))

(define (neighbors grid v)
  (let* ((adj '())
	 (push! (lambda (x)
		  (set! adj (cons x adj)))))
    (vector-for-each (lambda (x)
		       (when (vector-ref x v)
			     (push! (vector-ref x v))))
		     grid)))


;; We can create graphs with a depth-first-search algorithm