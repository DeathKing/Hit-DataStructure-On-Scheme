(load "list.scm")
(load "matrix.scm")
(load "graph-basic.scm")

(define (make-graph vertex edge)
  (let* ((s (length vertex)) (m (make-matrix s s 0)))
    (do ((e edge (cdr e)))
      ((null? e) m)
      (matrix-set! m (caar e) (cdar e) 1)
      (matrix-set! m (cdar e) (caar e) 1))
    (list 'graph vertex m)))

;;; This is for make directed graphics.
(define (make-digraph vertex edge)
  (let* ((s (length vertex)) (m (make-matrix s s 0)))
    (do ((e edge (cdr e)))
      ((null? e) m)
      (matrix-set! m (caar e) (cdar r) 1))
    (list 'digraph vertex m)))

(define (vertex-neighbors vertex graph)
  (let ((s (length (graph-vertex graph))))
    (let loop ((i 0) (neighbor '()))
      (if (= i s) neighbor
        (if (vertex-adjacent? vertex i graph)
          (loop (+ i 1) (append neighbor (list i)))
          (loop (+ i 1) neighbor))))))

(define (vertex-adjacent? v1 v2 graph)
  (let ((l (graph-vertex graph)))
    (if (node? v1) (set! v1 (list-index l v1)))
    (if (node? v2) (set! v2 (list-index l v2)))
    (= 1 (matrix-ref (graph-edge graph) v1 v2))))
