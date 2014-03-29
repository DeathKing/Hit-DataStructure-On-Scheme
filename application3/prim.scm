(load "graph.scm")

(define (mst-prim graph vertex)
  (let* ((vc (graph-vertex-count graph))
         (v (cons vertex '())) ;;; vertexs that already taken
         (e (graph-vertex-edges graph vertex)) ;;; edges that already taken
         (r (cons '() '()))
         (proc (lambda (x y) (< (edge-weight x) (edge-weight y)))))
    (let loop ((count 1)
               (ne (sort e proc)))
        (display v)
        (newline)
        (display r)
        (display ne)
      (if (= count vc)
        (cdr r)
        (let* ((se (car ne))
               (u (graph-vertex-ref graph (edge-vi se))))
          (format #t "u: ~A, v: ~A ~%" u v)
          (if (member u v)
            (loop count (cdr ne))
            (begin
              (append! r (list se))
              (append! v (list u))
              (append! ne (graph-vertex-edges graph u))
              (loop (+ count 1) (sort (cdr ne) proc)))))))
    (make-graph v (cdr r))))


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

(define pg (mst-prim g (cons 0 'A)))
(graph-plot pg "prim.dot")