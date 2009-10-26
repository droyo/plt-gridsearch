#lang scheme/gui

(require "graph.scm")
; The GUI for the Grid program

; Creates the frame for the GUI, labels it
(define frame (new frame% [label "Graph Search"]
                   [width 1000]
                   [height 1000]))

;Creates the Menu Bar
(define menu-bar
  (new menu-bar% [parent frame]))

; Creates the Menu Items
(new menu% [label "File"]	 
     [parent menu-bar])
(new menu% [label "Edit"]	 
     [parent menu-bar])
(new menu% [label "View"]	 
     [parent menu-bar])
(new menu% [label "Options"]	 
     [parent menu-bar])


(define grid (square-grid 8))
(define layout (force-layout grid .0003 10000 .4 .99))
; Define the procedure that draws the grid
(define (draw-grid dc)
  (let ((points (map (lambda (p)
                       (list (* (car p) 1000)
                             (* (cadr p) 500)))
                     layout))
        (draw-node (lambda (pt)
                     (send dc draw-rectangle
                           (car pt)
                           (cadr pt)
                           40 40))))
    (for-each draw-node points)))

;Define the Grid area
(define canvas
  (new canvas% [parent frame]
       [min-width 1000]
       [min-height 500]
       [paint-callback (lambda (canvas dc) (draw-grid dc))]))

; Get the canvas's drawing context
(define dc (send canvas get-dc))

; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define black-brush (make-object brush% "BLACK" 'solid))
(define white-brush (make-object brush% "WHITE" 'solid))
(define gray-brush (make-object brush% "GRAY" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))

;Make a panel to hold the message
(define vertpanel (new horizontal-panel% [parent frame]
                       [alignment '(center center)]))

; Defines a sample message
(define msg (new message% [parent vertpanel]
                 [label "THIS IS THE GRAPH / GRID THING"]))

; Make a panel to hold the buttons
  (define panel (new horizontal-panel% [parent frame]
                     [alignment '(center center)]))
  
; Make some buttons in the panel
(new button% [parent panel]
     [label "Start"]
    
     ; Callback procedure for a button click:
     (callback (lambda (button event)
                 (send msg set-label "GRAPHGRAPHGRAPH!"))))

(new button% [parent panel]
     [label "Stop"]
     
     ; Callback procedure for a button click:
     (callback (lambda (button event)
                 (send msg set-label "STOPSTOPSTOP!"))))

; Defines the interaction window (IW) panel
(define iw
  (new panel% [parent frame]
       [min-width 400]
       [min-height 100]
       [border 5]))

; Creates the IW message
(new message% 
     [label "This is where the text for the Interaction Window will, go, line by line, and can detail the progrss and debug information as the program runs.
Line 2
Line 3
Line 4
Line 5
Etc."]
     [parent iw])


; Shows the frame
(send frame show #t)

; Wait a second to let the window get ready
(sleep/yield 1)

; Draw the grid
(draw-grid dc)