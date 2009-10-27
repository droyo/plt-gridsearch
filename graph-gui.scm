;;;; Notes on the GUI: I want it to be very spartan, ideally a huge
;;;; canvas area with buttons and a slider along the top and
;;;; bottom. Text output can be given in the repl we run the gui
;;;; from. This is not meant to be a standalone application.
#lang scheme/gui

;(require "graph.scm" "graph-layout.scm")

;; Our main window
(define *top-frame*
  (new frame%
       [label "Graph Search"]
       [min-width 320]
       [min-height 200]))

(define *main-panel*
  (new vertical-panel%
       [parent *top-frame*]
       [alignment '(center top)]))

;; The graph area
(define *graph-canvas*
  (new canvas%
       [parent *main-panel*]
       [style '(border)]
       [stretchable-width #t]
       [stretchable-height #t]))

;; A horizontal bar of buttons and a slider
(define *button-bar* (new horizontal-panel%
			  [parent *main-panel*]
			  [alignment '(center bottom)]
			  [stretchable-height #f]))
  
;; Make some buttons in the panel
;; Start/Stop
(new button%
     [parent *button-bar*]
     [label "Start"])
(new button%
     [parent *button-bar*]
     [label "Stop"])
;; Slider bar
(new slider%
     [label "Speed"]
     [parent *button-bar*]
     [min-value 0]
     [max-value 1000])
;; Prev/Next buttons
(new button%
     [parent *button-bar*]
     [label "<"])
(new button%
     [parent *button-bar*]
     [label ">"])

(define (run)
  (send *top-frame* show #t);Shows the frame
  ;; Wait a second to let the window get ready
  (sleep/yield 1))
