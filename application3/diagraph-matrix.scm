;;; THE DIAGRAPH IMPLEMENT USING MATRIX
;;; 
;;; AUTHOR: DeathKing<dk#hit.edu.cn>
;;; LICENSE: MIT/HIT

(load "diagraph-basic.scm")
(load "../library/matrix.scm")
(load "../library/matrix-extend.scm")

(load-option 'format)

;;; MAKE-GRAPH: make a graph present by adjance-matrix.
;;;  vertices: a list of vertex
;;;     edges: a list of edge
(define (make-diagraph vertices edges)
  (let* ((s (length vertices))
         (m (make-matrix s s *edge-unreachable*)))
    (do ((es edges (cdr es)))
      ((null? es) m)
      (let ((e (car es)))
        (matrix-set! m (edge-ui e) (edge-vi e) (edge-weight e))))
    (cons vertices m)))

;;; GRAPH-EDGES: returns a list which contains all the edges of the graph
;;;  graph: the graph
(define (diagraph-edges diagraph)
  (let* ((r (cons '() '()))
         (m (diagraph-edge diagraph))
         (row (matrix-row-length (diagraph-edge diagraph))))
    (map (lambda (x)
      (map (lambda (y)
        (let ((data (matrix-ref m x y)))
          (if (not (eq? *edge-unreachable* data))
            (append! r (list (make-edge x y data))))))
        (iota row)))
      (iota row))
    (cdr r)))

(define (diagraph-vertex-edges diagraph vertex)
  (let* ((r (cons '() '()))
         (vc (diagraph-vertex-count diagraph))
         (ui (car vertex))
         (u (vector-ref (diagraph-edge diagraph) ui)))
    (for-each
      (lambda (vi)
        (let ((w (vector-ref u vi)))
          (if (not (eq? *edge-unreachable* w))
            (append! r (list (make-edge ui vi w))))))
      (iota vc))
    (cdr r)))

(define (diagraph-vertex-count diagraph)
  (length (diagraph-vertex diagraph)))

(define (diagraph-edge-count diagraph)
  (length (diagraph-edges diagraph)))

;;; VERTEX-NEIGHBORS: find all the neibors of a vertex
;;;     vi: the index of vertex v
;;;  graph: the graph
(define (vertex-neighbors vi diagraph)
  (let ((s (length (diagraph-vertex diagraph))))
    (let loop ((i 0) (neighbor '()))
      (if (= i s) neighbor
        (if (vertex-adjacent? vi i diagraph)
          (loop (+ i 1) (append neighbor (list i)))
          (loop (+ i 1) neighbor))))))

;;; VERTEX-ADJACENT: check whether the vertex u and v are adjancent
;;;     ui: the index of vertex u
;;;     vi: the index of vertex v
;;;  graph: the graph
(define (vertex-adjacent? ui vi diagraph)
  (not (eq? *edge-unreachable* (matrix-ref (diagraph-edge diagraph) ui vi))))

(define (diagraph-vertex-ref diagraph index)
  (list-ref (diagraph-vertex diagraph) index))

(define (diagraph-transpose diagraph)
  (let* ((v (diagraph-vertex diagraph))
         (m (diagraph-edge diagraph))
         (mt (matrix-copy m)))
    (matrix-each-with-index m
      (lambda (r c v)
        (matrix-set! mt v c r)))
    (cons v mt)))

(define (diagraph-plot diagraph file)
  
  (define (plot-edge e p)
    (let* ((ui (edge-ui e)) (ud (vertex-attribute (diagraph-vertex-ref diagraph ui)))
           (vi (edge-vi e)) (vd (vertex-attribute (diagraph-vertex-ref diagraph vi)))
           (weight (edge-weight e)))
      (format p "~A -> ~A [label=~A, shape=diamond];~%" ud vd weight)))

  (let ((port (open-output-file file)))
    (format port "digraph g {~%")
    (map (lambda (x) (plot-edge x port)) (diagraph-edges diagraph))
    (format port "}")
    (close-output-port port)))