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
    (do ((ver vertex (vector-index t #f)))
      ((not (any (lambda (x) (eq? x #f)) (vector->list t)))
       (list 'dfst seq te be))
      (let iter ((v ver))
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
          (vertex-neighbors v graph))))))


(define (graph->dfst/iter vertex graph)
  '())

(define (graph->bfst vertex graph)
  (let* ((be '()) (te '()) (seq '())
         (t (make-vector (length (graph-vertex graph)) #f))
         (visited? (lambda (x) (vector-ref t x))))
    (do ((ver vertex (vector-index t #f)))
      ; return till all visited.
      ((not (any (lambda (x) (eq? x #f)) (vector->list t)))
       (list 'bfst seq te be))
      (let loop ((queue (list ver)))
        ; Queue Empty
        (if (null? queue)
          (list 'dfst seq te be)
          (let ((v (list-ref queue 0)))
            (set! seq (append seq (list v)))
            ; Set visited.
            (vector-set! t v #t)
            (for-each
              (lambda (x)
                (if (visited? x)
                  (if (and (> (list-index seq v) (+ 1 (list-index seq x)))
                           (list-include? te (cons x v)))
                    (set! be (append be (list (cons v x)))))
                  (set! te (append te (list (cons v x))))))
              (vertex-neighbors v graph))
            (set! queue (append queue (filter (lambda (x)
                                                (and (not (visited? x))
                                                     (not (list-include? queue x))))
                                                (vertex-neighbors v graph))))
            (loop (cdr queue))))))))

(define (bfst-inspect bfst)
  (display "The traversal sequence is:")
  (display (list-ref bfst 1))
  (newline))
