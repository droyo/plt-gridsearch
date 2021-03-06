;;;; Drawing graph elements on the canvas
(module graph-draw scheme/gui
  (require srfi/1 srfi/39
	   "graph-create.scm"
	   "graph-layout.scm"
	   "helper-functions.scm")

  ;;; The following parameters are provided for the user to customize
  ;;; the program. You can set them by calling them like functions
  ;;; with their new value as an argument. Call them with no arguments
  ;;; to get their current value.
  ;;; (padding) -> 30
  ;;; (padding 23)
  ;;; (padding) -> 23

  ;; Margin between graph and edge of canvas (graph number is drawn here)
  (define padding (make-parameter 15))
  (define node-size (make-parameter 
                     30 (lambda (x);update padding when this is changed
			  (padding (/ x 2)) x)))

  (define player-size (make-parameter 20))
  
  ;; http://docs.plt-scheme.org/gui/pen-list_.html
  (define (pen color width)
    (send the-pen-list find-or-create-pen
	  color width 'solid))
  
  (define (brush color)
    (send the-brush-list find-or-create-brush
	  color 'solid))

  (define edge-pen (make-parameter (pen "Blue" 2)))
  (define node-pen (make-parameter (pen "Black" 1)))
  (define goal-pen (make-parameter (pen "Green" 1)))
  ;; color for node backgrounds
  (define background-brush (make-parameter (brush "SeaShell")))

  ;; True if we want player trails highlited
  (define highlight-player-trails (make-parameter #t))

  ;;; Drawing functions
  ;; draw-graph redraws the whole graph. only called on window refresh

  ;; Return a random color pen
  (define (random-pen width)
    (pen (random-choose '("Red" "RoyalBlue" "SteelBlue" "Dark Olive Green"
			  "DarkSeaGreen" "DarkKhaki" "Peru" "Sienna"
			  "Chocolate" "Orchid"))
	 width))

  ;; Consider introducing a proper data structure for edges
  (define (draw-graph dc graph-struct)
    (for-each (lambda (edge)
                (apply draw-edge dc (cdr edge)))
               (graph-edges graph-struct))
    (for-each (lambda (vertex pos)
                (apply draw-node dc
		       (number->string vertex) pos))
	      (vertices (graph-data graph-struct))
	      (graph-points graph-struct)))

  ;; Currently we draw nodes as black outlined circles with their names
  ;; in the middle.
  (define (draw-node dc name x y [pen (node-pen)])
      (send dc set-pen pen)
      (send dc draw-ellipse
	    (- x (/ (node-size) 2))
	    (- y (/ (node-size) 2))
	    (node-size) (node-size))
      (send dc draw-text name
	    ;; The name isn't centered properly
	    (- x (* 4 (remainder (string-length name) 6)))
	    (- y 8)))

  (define (draw-edge dc from to [pen (edge-pen)])
    (send dc set-pen pen)
    (send dc draw-line 
          (first from) (second from)
          (first to) (second to)))

  ;; Consider highlighting edges with player colors.
  ;; For now players/goals are rounded rectangles
  (define (draw-player dc graph-struct player)
    (let* ((trail (map (compose (lookup-positions graph-struct))
		       (player-trail player)))
	   (pos (first trail))
	   (breadcrumbs (lambda (a b)
			  (draw-edge dc a b
				     (player-pen player))
			  a)))
      (when (highlight-player-trails)
	    (fold breadcrumbs pos trail))

      (send dc set-pen (player-pen player))
      (send dc set-brush (brush (send (player-pen player) get-color)))

      (send dc draw-rounded-rectangle 
            (- (first pos) (/ (player-size) 2))
            (- (second pos)(/ (player-size) 2))
            (player-size) (player-size))

      (send dc set-brush (background-brush))))

  (provide padding
	   node-size
	   player-size
	   background-brush
	   edge-pen
	   node-pen
	   goal-pen
	   random-pen
	   draw-graph
	   draw-node
	   draw-player
	   draw-edge))