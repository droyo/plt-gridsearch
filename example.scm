#lang scheme

;; Example usage of our graph program
(require srfi/1 "graph.scm" "graph-layout.scm" 
         "graph-display.scm" "helper-functions.scm")

;; Choose our layout function. Current possible choices are:
;; grid-layout, random-layout
(layout-function grid-layout)

;; Choose how our graph will be created. See graph.scm for graph
;; generation functions.
(new-graph-function
 (lambda ()
   (kruskal-maze (square-grid 15) .3)))

;; Choose how big we want various elements to be drawn on the
;; display. Defaults are in graph-gui.scm
(node-size 25)
(player-size 18)

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
;; - A function that gives the coordinates of a vertex on the screen

;; Note that the goal function works double-duty; when called with no
;; arguments it returns a list of goals. This may or may not change in
;; the future if it proves too confusing.

;; The graph software takes care of marking nodes visited, keeping a
;; visited list for each player rather than marking the graph, so that
;; multiple players can search the same graph without interference.
;; Our search function returns the next vertex to go to. The graph
;; software calls it repeatedly until it returns the value 'finished.
;; If a function determines there is no path to the goal, it can
;; return 'no-path

;; A reference random-search
(define (random-search start adjacent visited? goal? coordinates)
  (let* ((adj (adjacent start))
	 (new-adj (remove visited? adj)))
    (cond ((any goal? (adjacent start))
	   (find goal? (adjacent start)))
	  ((not (null? new-adj))
	   (random-choose new-adj))
	  ((not (null? adj))
	   (random-choose adj))
	  (else
	   'no-path))))

;; If our search function returns a list, it will not be called again.
;; Instead the graph software will step the player over each element 
;; of the list it returns, if they form a path. When it reaches the 
;; end of the list, or the goal, the player will be deleted.

;; A reference DFS search. We use recursion as our stack here.
(define (depth-first-search start adjacent visited? goal? coordinates)
  ;; Note: visited? only returns #t if we have physically been to 
  ;; a vertex. Because this dfs does not actually *move* the player,
  ;; we need our own way of keeping track of old nodes.
  (define old '())
  (define (mark v) (push! v old))
  (define (old? v) (member v old))
  (define goal (find goal? (goal?)))
  (define (expand v) 
    (mark v)
    (sort (remove old? (adjacent v))
          (lambda (v1 v2)
            (< (dist-squared (coordinates v1) 
                             (coordinates goal))
               (dist-squared (coordinates v2) 
                             (coordinates goal))))))
  
  (let dfs ((adj (expand start)))
    (cond ((null? adj) #f)
          ((find goal? adj) => list)
          ((dfs (expand (first adj)))
           => (lambda (path)
                (cons (first adj) path)))
          (else (dfs (cdr adj))))))

;; The most straightforward way to code BFS is using a queue.  The
;; file helper-functions.scm provides the pop! and add! macros, which,
;; together with 'first' (or car) will provide you with queue
;; operations. Here is an example.
(printf "Queue demonstration~%")
(let loop ((q '(a b c 1 2 3)))
  (unless (eq? (first q) 3)
    ;; Off the front, onto the back
    (add! (first q) q)
    (pop! q)
    (printf "q = ~A~%" q)
    (loop q)))

;;  Add a player to the graph. The arguments are: A
;; name(string), a search function as described above, and an optional
;; starting point (if not supplied a random one is chosen)
(graph-session 'add-player "Joseph" depth-first-search)
(graph-session 'add-player "Harry" random-search)

;; There is a button for this, but we may start/pause the animation from
;; within our program.
(graph-session 'toggle-start)
