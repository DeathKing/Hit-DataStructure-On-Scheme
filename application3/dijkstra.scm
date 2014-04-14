(load "diagraph.scm")

(define (dijkstra diagraph u)
  (let* ((s (cons u '()))
         (vc (diagraph-vertex-count diagraph))
         (d (make-vector vc *edge-unreachable*))
         (edges (diagraph-vertex-edges diagraph u))
         (proc (lambda (x y) (< (edge-weight x) (edge-weight y)))))  ;;; proc that compare each edges
    (vector-set! d (vertex-data u) 0)
    (for-each (lambda (e)
      (if (not (eq? *edge-unreachable* (edge-weight e)))
        (vector-set! d (edge-vi e) (edge-weight e))))
      edges)
    (let loop ((es (sort edges proc)))
      (if (= (diagraph-vertex-count diagraph) (length s))
        '()
        (let* ((e (car es)) 
               (ui (edge-ui e))
               (u (diagraph-vertex-ref diagraph ui)))
          (for-each
            (lambda (x)
              (let ((weight (edge-weight x))
                    (vi (edge-vi x)))
                (if (and (not (eq? *edge-unreachable* weight))
                         (> (vector-ref d vi)
                            (+ (vector-ref d ui) weight)))
                  (vector-set! d vi (+ (vector-ref d ui) weight)))))
            (diagraph-vertex-edges diagraph u))
          (append! s (list u))
          (loop (sort (cdr (append edges (diagraph-vertex-edges diagraph u))) proc)))))
    (for-each
      (lambda (x)
        (let ((weight (vector-ref d x))
              (ud (vertex-attribute u))
              (vd (vertex-attribute (diagraph-vertex-ref diagraph x))))
          (if (not (eq? *edge-unreachable* weight))
            (format #t "~A to ~A costs ~A.~%" ud vd weight))))
      (iota vc))))



(define lvs
  (list (cons 0 1) (cons 1 2) (cons 2 3) (cons 3 4) (cons 4 5) (cons 5 6)))

(define egs
  (list
    (list 0 1 10) (list 0 3 30) (list 0 4 100)
    (list 1 2 50)
    (list 2 3 20) (list 2 4 10)
    (list 3 2 20) (list 3 4 60)))

(define v (map pair->vertex lvs))
(define e (map list->edge egs))
(define g (make-diagraph v e))

(diagraph-plot g "dijkstra.dot")
(dijkstra g (cons 0 1))