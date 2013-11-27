;; Matrices.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (make-matrix d1 d2 . fill)
  (if (null? fill)
      (list->vector
       (map (lambda (undef) (make-vector d2))
            (vector->list (make-vector d1))))
      (if (null? (cdr fill))
          (list->vector
           (map (lambda (undef) (make-vector d2 (car fill)))
                (vector->list (make-vector d1))))
          (error "Only one fill value allowed -- MAKE-MATRIX"
                 fill))))
          
(define (matrix-ref matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (matrix-set! matrix i j v)
  (vector-set! (vector-ref matrix i) j v))

(define (matrix-row-length matrix)
  (vector-length (vector-ref matrix 0)))

(define (matrix-col-length matrix)
  (vector-length matrix))

(define (matrix-row-copy matrix i)
  (define r (matrix-row-length matrix))
  (let ((copy (make-vector r)))
    (let loop ((j 0))
      (if (< j r)
          (begin
           (vector-set! copy j (matrix-ref matrix i j))
           (loop (+ j 1)))
          copy))))

(define (matrix-col-copy matrix j)
  (define c (matrix-col-length matrix))
  (let ((copy (make-vector c)))
    (let loop ((i 0))
      (if (< i c)
          (begin
           (vector-set! copy i (matrix-ref matrix i j))
           (loop (+ i 1)))
          copy))))

(define (matrix-copy matrix)
  (define r (matrix-row-length matrix))
  (define c (matrix-col-length matrix))
  (let ((copy (make-vector r)))
    (let loop ((i 0))
      (if (< i r)
          (begin
           (vector-set! copy i (matrix-row-copy matrix i))
           (loop (+ i 1)))
          copy))))
