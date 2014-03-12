;;; GRAPH-LIST
;;; Build graph using adjacency list.

(load "../library/list.scm")
(load "graph-basic.scm")

(define (make-graph vertex edge)
  (let* ((v (make-vector (length vertex) '())))
    (do ((e edge (cdr e)))
      ((null? e) v)
      (vector-set! v (caar e) (append (vector-ref v (caar e)) (list (cdar e))))
      (vector-set! v (cdar e) (append (vector-ref v (cdar e)) (list (caar e)))))
    (list 'graph vertex v)))

(define (make-digraph vertex edge)
  (let* ((v (make-vector (length vertex) '())))
    (do ((e edge (cdr edge)))
      ((null? e) v)
      (vector-set! v (caar e) (append (vector-ref v (caar e)) (list (cdar e)))))
    (list 'digraph vertex v)))

(define (vertex-neighbors vertex graph)
  (vector-ref (list-ref graph 2) vertex))

(define (vertex-adjacent? v1 v2 graph)
  (let ((l (graph-vertex graph))
        (v (list-ref graph 2)))
    (if (node? v1) (set! v1 (list-index l v1)))
    (if (node? v2) (set! v2 (list-index l v2)))
    (list-member? (vector-ref v v1) v2)))
