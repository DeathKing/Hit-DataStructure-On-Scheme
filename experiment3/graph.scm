;;; Graph
;;;
;;; Written by: DeathKing<dk@hit.edu.cn>

(load "graph-list.scm")
;(load "graph-matrix.scm")

(define (graph-dfs vertex graph proc)
  (define t (make-list (length (graph-vertex graph)) #f))
  (define (iter v)
    (proc v graph)
    (list-set! t v #t)
    (for-each
      (lambda (x) (if (not (list-ref t x)) (iter x)))
      (vertex-neighbors v graph)))
  (iter vertex))

(define (graph-dfs/iter vertex graph proc)
  '())

(define (graph-bfs vertex graph proc)
  (define t (make-list (length (graph-vertex graph)) #f))
  (define (iter v open)))

