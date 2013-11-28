;; Floyd's shortest path algorithm.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (floyd1 C)
  
  ; computes the shortest path matrix corresponding
  ; to the cost matrix C
  
  (define n (matrix-row-length C))
  
  (define A (matrix-copy C))
    ; shortest path matrix
  
  ; floyd
  (let loop1 ((k 0))
    (if (< k n)
        (let loop2 ((i 0))
          (if (< i n)
              (let loop3 ((j 0))
                (if (< j n)
                    (begin
                     (if (< (+ (matrix-ref A i k)
                               (matrix-ref A k j))
                            (matrix-ref A i j))
                         (matrix-set! A i j
                           (+ (matrix-ref A i k)
                              (matrix-ref A k j))))
                     (loop3 (+ j 1)))
                    (loop2 (+ i 1))))
              (loop1 (+ k 1))))
        A)))

(define (floyd2 C)
  
  ; computes a pair that contains
  ; (1) the shortest path matrix and 
  ; (2) the "middle" point matrix 
  ; corresponding to the cost matrix C
  
  (define n (matrix-row-length C))
  
  (define A (matrix-copy C))
    ; shortest path matrix
  
  (define P (make-matrix n n -1))
    ; middle point matrix
  
  ; floyd2
  (let loop1 ((k 0))
    (if (< k n)
        (let loop2 ((i 0))
          (if (< i n)
              (let loop3 ((j 0))
                (if (< j n)
                    (begin
                     (if (< (+ (matrix-ref A i k)
                               (matrix-ref A k j))
                            (matrix-ref A i j))
                         (begin
                           (matrix-set! A i j
                             (+ (matrix-ref A i k)
                                (matrix-ref A k j)))
                           (matrix-set! P i j k)))
                     (loop3 (+ j 1))) 
                    (loop2 (+ i 1))))
              (loop1 (+ k 1))))
        (cons A P))))

(define A 
  (make-cost-matrix 3
    '((0 0 2) (0 1 8) (0 2 5) (1 0 3) (2 1 2))))