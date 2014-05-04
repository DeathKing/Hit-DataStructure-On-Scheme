;;; SIMPLE VECTOR EXTENSION

(define (vector-swap! vec i j)
  (let ((x (vector-ref vec i))
        (y (vector-ref vec j)))
    (vector-set! vec i y)
    (vector-set! vec j x)))