(load "../application2/disjoint-set.scm")
(load "graph.scm")

(define (mst-kruskal g)
  (let* ((vc (graph-vertex-count g))
         (ec (graph-edge-count g))
         (se (cons '() '()))
         (ds (map
          (lambda (v) (make-set (vertex-data v))) (graph-vertex g))))
    (let loop
      ((count 0)
       (edges (sort (graph-edges g) (lambda (i j) (< (edge-weight i) (edge-weight j))))))
      (if (or (null? edges) (= count (- vc 1)))
          (make-graph (graph-vertex g) (cdr se))
        (let* ((e (car edges))
               (u (list-ref ds (edge-ui e)))
               (v (list-ref ds (edge-vi e))))
          (if (set-equal? u v)
            (loop count (cdr edges))
            (begin
              (set-union! u v)
              (append! se (list e))
              (loop (+ 1 count) (cdr edges)))))))))


(define lvs
  (list (cons 0 'A) (cons 1 'B) (cons 2 'C) (cons 3 'D) (cons 4 'E) (cons 5 'F)))

(define egs
  (list
    (list 0 1 6) (list 0 2 1) (list 0 3 5)
    (list 1 2 5) (list 1 4 3)
    (list 2 3 5) (list 2 4 6) (list 2 5 4)
    (list 3 5 2)
    (list 4 5 6)))

(define v (map pair->vertex lvs))
(define e (map list->edge egs))
(define g (make-graph v e))

;(define srte (sort (graph-edges g) (lambda (i j) (< (edge-weight i) (edge-weight j)))))
(define sg (mst-kruskal g))
(graph-plot sg "kruskal.dot")