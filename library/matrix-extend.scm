;; MATRIX-EXTENSION

(define (matrix-each-with-index matrix proc)
  (let ((r (matrix-row-length matrix))
        (c (matrix-col-length matrix)))
    (map (lambda (x)
      (map (lambda (y)
        (proc (matrix-ref matrix x y) x y))
      (iota c)))
    (iota r))))

(define (matrix-each matrix proc)
  (let ((r (matrix-row-length matrix))
        (c (matrix-col-length matrix)))
    (map (lambda (x)
      (map (lambda (y)
        (proc (matrix-ref matrix x y)))
      (iota c)))
    (iota r))))
