;;;; A Graph data structure, along with functions to generate
;;;; fully-connected graphs. The graphs used in this application will
;;;; be pretty dense, so for now we will be using a simple WxH
;;;; adjacency matrix, where rows and columns represent vertexes, and
;;;; their intersections have the value #f if there is no edge between
;;;; them, or a positive number indicating a weight.


(define (2d-vector width height fill)
  (make-vector height
	       (make-vector width fill)))

(define (disconnected-graph width height)
  (2d-vector width height #f))

(define (fully-connected-graph width height)
  (2d-vector width height 0))

;; We can create graphs with a depth-first-search algorithm