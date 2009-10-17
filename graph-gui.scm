#lang scheme/gui

; Creates the frame for the GUI, labels it
(define frame (new frame% [label "Graph Search"]))

; Defines a sample message
(define msg (new message% [parent frame]
                 [label "THIS IS THE GRAPH / GRID THING"]))
  
; Make a button in the frame
(new button% [parent frame]
     [label "Click Me Now!"]
     ; Callback procedure for a button click:
     (callback (lambda (button event)
                 (send msg set-label "GRAPHGRAPHGRAPH!"))))

; Shows the frame
(send frame show #t)