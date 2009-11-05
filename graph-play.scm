(define-module graph-play scheme
  (require srfi/1 srfi/9 "graph-create.scm")

  ;; Graph-session holds a graph, players navigating through the
  ;; graph, and list of goals on the graph
  (define-record-type :graph-session
    (make-graph-session graph)
    graph-session?
    (graph   session-graph)
    (players session-players set-session-players!)
    (goals   session-goals   set-session-goals!))
  
  (define-record-type :player
    (make-player search name)
    player?
    (search player-search)
    (name   player-name)
    (trail  player-trail set-player-trail!))

  (define-record-type :goal
    (make-goal vertex)
    goal-type?
    (vertex goal-position)
    (found  hall-of-fame))

  (define (add-player s player)
    (set-session-players! s (cons player
				  (session-players s))))

  (define (add-goal s goal)
    (set-session-goals! s (cons goal
				(session-goals s))))
  
  )
