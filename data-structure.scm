#lang scheme

;;;; A Graph data structure, along with functions to generate
;;;; fully-connected graphs. The graphs used in this application will
;;;; be pretty dense, so for now we will be using a simple WxH
;;;; adjacency matrix, where rows and columns represent vertexes, and
;;;; their intersections have the value #f if there is no edge between
;;;; them, or a positive number indicating a weight.
(require srfi/1 srfi/43)

;;; Note about the graph data structure: be sure to use only the
;;; defined access functions to manipulate the graph. Do not use the
;;; inherent vector-based structure of the graph as this may change in
;;; the future to support larger graphs with less memory.

;; A graph with n vertices
(define (graph n connected?)
  (list->vector
   (map (lambda (v)
	  (list->vector (list-tabulate n (lambda (x)
					   (and (not (= x v)) connected?)))))
	(list-tabulate n values))))

(define graph-size
  vector-length)

(define (vertices graph)
  (vector->list (vector-map (lambda (i w) i) graph)))

;; All vertices directly reachable from v
(define (neighbors graph v)
  (let ((connections (vector-map (lambda (idx val) (and val idx))
				 (vector-ref graph v))))
    (filter values (vector->list connections))))

(define (modify! graph v1 v2 x)
  (cond ((not (< -1 v1 (graph-size graph)))
	 (error "The vertex does not exist" v1))
	((not (< -1 v2 (graph-size graph)))
	 (error "The vertex does not exist" v2))
	(else
	 (vector-set! (vector-ref graph v1) v2 x)
	 (vector-set! (vector-ref graph v2) v1 x)
	 graph)))

(define (connect! graph v1 v2)
  (modify! graph v1 v2 #t))

(define (disconnect! graph v1 v2)
  (modify! graph v1 v2 #f))

;; This shuffle takes an undefined amount of instructions.
(define (shuffle lst)
  (let f ((order '()))
    (if (= (length order) (length lst))
	(map (lambda (idx) (list-ref lst idx))
	     order)
	(f (lset-union = (list (random (length lst)))
		       order)))))

;; A reference depth-first-search algorithm
(define (search graph start end)
  (define visited '())

  (define (mark v)
    (set! visited (cons v visited)))

  (define (expand v)
    (mark v)
    (shuffle (lset-difference = (neighbors graph v) visited)))

  (let dfs ((now start)
	    (adj (expand start)))
    (cond ((member end adj)
	   (list now end))
	  ((null? adj) #f)
	  ((dfs (car adj) (expand (car adj)))
	   => (lambda (path)
		(cons now path)))
	  (else
	   (dfs now (cdr adj))))))

;;;; Layout algorithms There are many, many ways to layout a
;;;; graph. For our purposes, and for now, we will use a simple
;;;; force-directed layout. See the wikipedia page on force-based
;;;; algorithms for details

;;; layout takes a graph, a function to draw a node, and a function to
;;; draw a connection between two nodes, in addition to screen
;;; dimensions. This is only called when the screen is opened or
;;; resized.
(define +spring+ .06)
(define +damping+ .86)
(define +threshold+ .4)
(define +standard-mass+ 1)

(define-syntax inc!
  (syntax-rules ()
    ((inc! var n)
     (begin (set! var (+ var n)) var))
    ((inc! var)
     (inc! var 1))))

;; multiply or scale points
(define (v* . pts)
  (fold (lambda (p1 p2)
	  (cond ((and (list? p1) (list? p2))
		 (list (* (car p1) (car p2))
		       (* (cadr p1) (cadr p2))))
		((list? p1)
		 (list (* (car p1) p2)
		       (* (cadr p1) p2)))
		((list? p2)
		 (list (* (car p2) p1)
		       (* (cadr p2) p1)))
		(else (* p1 p2))))
	1 pts))

(define (v+ . pts)
  (fold (lambda (p1 p2)
	  (list (+ (car p1) (car p2))
		(+ (cadr p1) (cadr p2))))
	(list 0 0) pts))

(define (v- . pts)
  (fold (lambda (p1 p2)
	  (list (- (car p1) (car p2))
		(- (cadr p1) (cadr p2))))
	(list 0 0) pts))

;;; This function is very ugly. However, in the interest of getting
;;; things done, I will stop obsessing over it and move on to other
;;; things for now.

;; produce a list of the form ((v0 (x0 y0)) (v1 (x1 y1)) ...) for easy
;; drawing, using a force-directed layout algorithm taken off of
;; wikipedia
(define (layout graph w h)
  (do ((t 0 (+ t 1))
       (energy 0)
       (vtx (vertices graph))
       (vel (make-vector (graph-size graph) '(0 0)))
       (pos (make-vector (graph-size graph) '(0 0))))
      ((and (> t 0) (< energy +threshold+))
	(map list vtx (vector->list pos)))
       (for-each (lambda (node)
		   (let ((net-force '(0 0)))
		     
		     (set! net-force
			   (apply v+ net-force
				  (map (lambda (other)
					 (repulsion (vector-ref pos node)
						    (vector-ref pos other)))
				       (delete node vtx))))
		     (set! net-force
			   (apply v+ net-force
				  (map (lambda (other)
					 (attraction (vector-ref pos node)
						     (vector-ref pos other)))
				       (neighbors graph node))))

		     (vector-set! vel node
				  (v* (v+ (vector-ref vel node)
					  (v* t net-force))
				      +damping+))
		   
		     (vector-set! pos node
				  (v+ (vector-ref pos node)
				      (v* t (vector-ref vel node))))
		   
		     (inc! energy (apply + (v* +standard-mass+
					       (vector-ref vel node)
					       (vector-ref vel node))))
		     (printf "~A~%" energy)))
		 vtx)))

;;; coulomb repulsion
(define (repulsion v1 v2)
  (let* ((diff (v- v1 v2))
	 (dist (+ .01 (sqrt (apply + (v* diff diff))))))
    (v* diff
	(/ 1 (* 4 pi))
	(/ 1 (expt dist 3)))))

;;; hooke attraction
(define (attraction v1 v2)
  (let* ((diff (v- v2 v1))
	 (dist (+ .01 (sqrt (apply + (v* diff diff))))))
    (v* -1 +spring+ dist diff (/ 1 dist))))

(define (init-pt nodes width height)
  (let loop ((pos '(#t #t)))
    (if (unique? pos)
	pos
	(loop (map (lambda (_) (list (random width)
				     (random height)))
		   nodes)))))

;; Check that the list has no duplicates
(define (unique? lst)
  (cond ((null? lst) #t)
	((member (car lst) (cdr lst)) #f)
	(else (unique? (cdr lst)))))
