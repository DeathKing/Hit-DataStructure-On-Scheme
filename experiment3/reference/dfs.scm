;;;;
;;;; Depth first search of a connected graph
;;;;
;;;; Copyright Pertti Kellom\"aki, pk@cs.tut.fi, 1992
;;;; This file may be freely distributed.
;;;;

(define (dfs graph visit-proc)

  (define visited '())

  (define (visit-vertex current)
    (cond ((member current visited)
           #f)
          (else
           ;; Handle this vertex
           (visit-proc current)
           (set! visited (cons current visited))
           ;; And its neighbors
           (let loop ((neighbors (vertex-neighbors current graph)))
             (cond ((null? neighbors)
                    #f)
                   (else
                    (visit-vertex (car neighbors))
                    (loop (cdr neighbors))))))))

  ;; start with the first vertex
  (visit-vertex (car (graph-vertices graph))))