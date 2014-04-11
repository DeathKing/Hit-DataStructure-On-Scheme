;;; THE GRAPH IMPLEMENT USING MATRIX
;;; 
;;; AUTHOR: DeathKing<dk#hit.edu.cn>
;;; LICENSE: MIT/HIT

(load "graph-basic.scm")
(load "../library/matrix.scm")
(load "../library/matrix-extend.scm")

(load-option 'format)

;;; MAKE-GRAPH: make a graph present by adjance-matrix.
;;;  vertices: a list of vertex
;;;     edges: a list of edge
(define (make-graph vertices edges)
  (let* ((s (length vertices))
         (m (make-matrix s s *edge-unreachable*)))
    (do ((es edges (cdr es)))
      ((null? es) m)
      (let ((e (car es)))
        (matrix-set! m (edge-ui e) (edge-vi e) (edge-weight e))
        (matrix-set! m (edge-vi e) (edge-ui e) (edge-weight e))))
    (cons vertices m)))

;;; GRAPH-EDGES: returns a list which contains all the edges of the graph
;;;  graph: the graph
(define (graph-edges graph)
  (let* ((r (cons '() '()))
         (m (graph-edge graph))
         (row (matrix-row-length (graph-edge graph))))
    (map
      (lambda (x)
        (map
          (lambda (y)
            (let ((data (matrix-ref m x y)))
              (if (not (eq? *edge-unreachable* data))
                (append! r (list (make-edge x y data))))))
          (iota x)))
      (iota row))
    (cdr r)))

(define (graph-vertex-edges graph vertex)
  (let* ((r (cons '() '()))
         (vc (graph-vertex-count graph))
         (ui (car vertex))
         (u (vector-ref (graph-edge graph) ui)))
    (for-each
      (lambda (vi)
        (let ((w (vector-ref u vi)))
          (if (not (eq? *edge-unreachable* w))
            (append! r (list (make-edge ui vi w))))))
      (iota vc))
    (cdr r)))

(define (graph-vertex-count graph)
  (length (graph-vertex graph)))

(define (graph-edge-count graph)
  (length (graph-edges graph)))

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
  (eq? *edge-unreachable* (matrix-ref (graph-edge graph) ui vi)))

(define (graph-vertex-ref graph index)
  (list-ref (graph-vertex graph) index))

(define (graph-plot graph file)
  
  (define (plot-edge e p)
    (let* ((ui (edge-ui e)) (ud (vertex-attribute (graph-vertex-ref graph ui)))
           (vi (edge-vi e)) (vd (vertex-attribute (graph-vertex-ref graph vi)))
           (weight (edge-weight e)))
      (format p "~A -- ~A [label=~A];~%" ud vd weight)))

  (let ((port (open-output-file file)))
    (format port "graph g {~%")
    (map (lambda (x) (plot-edge x port)) (graph-edges graph))
    (format port "}")
    (close-output-port port)))