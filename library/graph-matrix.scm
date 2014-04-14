(load "../library/graph-basic.scm")
(load "../library/matrix.scm")
(load "../library/matrix-extend.scm")

;;; MAKE-GRAPH: make a graph present by adjance-matrix.
;;;  vertexs: a list of vertex
;;;    edges: a list of edge
(define (make-graph vertexs edges)
  (let* ((s (length vertexs))
         (m (make-matrix s s 0)))
    (do ((es edges (cdr es)))
      ((null? es) m)
      (let ((e (car es)))
        (matrix-set! m (edge-ui e) (edge-vi e) (edge-weight e))
        (matrix-set! m (edge-vi e) (edge-ui e) (edge-weight e))))
    (cons vertexs m)))

;;; GRAPH-EDGES: returns a list which contains all the edges of the graph
;;;  graph: the graph
(define (graph-edges graph)
  (let ((r (cons '() '())))
    (matrix-each-with-index (graph-edge graph)
      (lambda (data row col)
        (if (not (eq? *edge-unreachable* data))
          (append! r (list (make-edge row col data))))))
    (cdr r)))

(define (graph-vertex-count graph)
  (length (graph-vertex graph)))

(define (graph-edge-count graph)
  (length (graph-edge graph)))

;;; VERTEX-NEIGHBORS: find all the neibors of a vertex
;;;     vi: the index of vertex v
;;;  graph: the graph
(define (vertex-neighbors vi graph)
  (let ((s (length (graph-vertex graph))))
    (let loop ((i 0) (neighbor '()))
      (if (= i s) neighbor
        (if (vertex-adjacent? vi i graph)
          (loop (+ i 1) (append neighbor (list i)))
          (loop (+ i 1) neighbor))))))

;;; VERTEX-ADJACENT: check whether the vertex u and v are adjancent
;;;     ui: the index of vertex u
;;;     vi: the index of vertex v
;;;  graph: the graph
(define (vertex-adjacent? ui vi graph)
  (eq? *edge-unreachable* (matrix-ref (graph-edge graph) ui vi))))