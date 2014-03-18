;;; SIMPLE HEAP SORT ALGORITHM
;;;
;;; AUTHOR:  Juha Heinanen
;;; LICENSE: Copyright Juha Heinanen 1988

(load "../library/vector.scm")

(define (heap-sort! vec less?)
  ; sorts vector vec according to less?
  (define (siftup n)
    (let loop ((child n))
      (if (> child 0)
          (let ((parent (quotient (- child 1) 2)))
            (if (not (less? (vector-ref vec child)
                            (vector-ref vec parent)))
                (begin
                  (vector-swap! vec child parent)
                  (loop parent)))))))
  (define (siftdown n)
    (let loop ((parent 0))
      (if (< parent n)
          (let ((left-child (+ (* 2 parent) 1)))
            (if (<= left-child n)
                (let ((larger-child
                       (if (and (< left-child n)
                                (less? 
                                  (vector-ref vec left-child)
                                  (vector-ref vec (+ left-child 1))))
                           (+ left-child 1)
                           left-child)))
                  (if (not (less? (vector-ref vec larger-child)
                                  (vector-ref vec parent)))
                      (begin
                       (vector-swap! vec larger-child parent)
                       (loop larger-child)))))))))
  ; heap-sort!
  (let ((last-index (- (vector-length vec) 1)))
    (let loop ((index 1))
      (if (<= index last-index)
          (begin
            (siftup index)
            (loop (+ index 1)))))
    (let loop ((index last-index))
      (if (> index 0)
          (begin
            (vector-swap! vec 0 index)
            (siftdown (- index 1))
            (loop (- index 1)))))))

;;; Test Code
(define v (make-vector 16 0))
(for-each
  (lambda (x) (vector-set! v x (random 100)))
  (iota 16))

(newline)
(display "The origin vector:")
(display v)
(newline)
(display "After heap sort:")
(heap-sort! v <)
(display v)
