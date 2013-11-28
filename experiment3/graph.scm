;;; Graph
;;;
;;; Written by: DeathKing<dk@hit.edu.cn>

;(load "graph-list.scm")
(load "graph-matrix.scm")
(load-option 'format)

(define (graph->dfst vertex graph)
  (let* ((be '()) (te '()) (seq '())
         (t (make-vector (length (graph-vertex graph)) #f))
         (visited? (lambda (x) (vector-ref t x))))
    (let iter ((v vertex))
      (set! seq (append seq (list v)))
      (vector-set! t v #t)
      (for-each
        (lambda (x)
          (if (visited? x)
            (if (and (> (list-index seq v) (+ 1 (list-index seq x)))
                     (list-include? te (cons x v)))
              (set! be (append be (list (cons v x)))))
            (begin
              (set! te (append te (list (cons v x))))
              (iter x))))
        (vertex-neighbors v graph)))
    (list 'dfst seq te be)))
 
(define (graph->dfst/iter vertex graph)
  '())

(define (graph->bfst vertex graph)
  '())
