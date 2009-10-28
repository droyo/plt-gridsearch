#lang scheme

;; Example usage of our graph program
(require srfi/1 "graph.scm" "graph-layout.scm" "graph-search.scm")

;; Choose our layout function. Current possible choices are:
;; grid-layout, random-layout
(layout-function grid-layout)

;; Choose how our graph will be created. See graph.scm for graph
;; generation functions.
(new-graph-function
 (lambda ()
   (square-grid 8)))

;; Choose how big we want various elements to be drawn on the
;; display. Defaults are in graph-gui.scm
(node-size 35)
(player-size 18)
(goal-size 24)

;; Setup our graph and open our visualization window. This returns a
;; function that parses messages and changes the state of the
;; graph. This is how we put players and goals and new connections
;; into the graph.
(define graph-session (display-graph))

;; Add in our goal. Passing no arguments chooses a random vertex for a
;; goal.
(graph-session 'add-goal)

;; A search function. Search functions are passed the following args:

;; - The player's current position
;; - A function that gets the adjacent vertices of a given vertex.
;; - A function that checks if a vertex is visited or not.
;; - A function that checks if a vertex is a goal.

;; The graph software takes care of marking nodes visited, keeping a
;; visited list for each player rather than marking the graph, so that
;; multiple players can search the same graph without interference.
;; Our search function returns the next vertex to go to. The graph
;; software calls it repeatedly until it returns the value 'finished.
;; If a function determines there is no path to the goal, it can
;; return 'no-path

;; A reference random-search
(define (random-search start adjacent visited? goal?)
  (let* ((adj (adjacent start))
	 (new-adj (remove visited? adj)))
    (cond ((goal? start)
	   'finished)
	  ((any goal? (adjacent start))
	   (find goal? (adjacent start)))
	  ((not (null? new-adj))
	   (list-ref new-adj (random (length new-adj))))
	  ((not (null? adj))
	   (list-ref adj (random (length adj))))
	  (else
	   'no-path))))

;; Add a player to the graph. The arguments are: A name(string), a
;; search function as described above, and an optional starting point
;; (if not supplied a random one is chosen)
(graph-session 'add-player "Randy" random-search)

;; There is a button for this, but we may start/pause the animation from
;; within our program.
(graph-session 'toggle-start)
