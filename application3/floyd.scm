(load "diagraph.scm")

(define (floyd diagraph)
  (let ((m (matrix-copy (diagraph-edge diagraph)))
        (c (diagraph-vertex-count diagraph)))
    (for-each (lambda (i)
      (for-each (lambda (j)
        (for-each (lambda (k)
          (let ((a (matrix-ref m i k))
                (b (matrix-ref m k j))
                (c (matrix-ref m i j)))
            (if (or (or (eq? *edge-unreachable* a) (eq? *edge-unreachable* b) (eq? *edge-unreachable* c))
                    (>= (+ a b) c))
              '()
              (matrix-set! m i j (+ a b)))))
          (iota c)))
        (iota c)))
      (iota c))
    (matrix-each-with-index m
      (lambda (x row col)
        (if (eq? *edge-unreachable* x)
          '()
          (let* ((ud (vertex-attribute (diagraph-vertex-ref diagraph row)))
                 (vd (vertex-attribute (diagraph-vertex-ref diagraph col))))
            (format #t "~A to ~A cost ~A.~%" ud vd x))))))) 

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

(diagraph-plot g "floyd.dot")
(floyd g)