;; Heap sort.

(define *default-heap-size* 100)

(define (make-heap #!optional proc size)
  (cons (cons 0 proc)
    (make-vector (if (eq? #!default size) *default-heap-size* size) '())))

(define (heap-size heap)
  (+ 1 (caar heap)))

(define (heap-pointer heap)
  (caar heap))

(define (heap-proc heap)
  (cdar heap))

(define (heap-data heap)
  (cadr heap))

(define (heap-maxsize heap)
  (vector-length (heap-data heap)))

(define (heap-size-set! heap value)
  (set-car! (car heap)))

(define (heap-size-inc! heap)
  (heap-size-set! heap (+ (heap-size heap) 1)))

(define (heap-size-dec! heap)
  (heap-size-set! heap (- (heap-size heap) 1)))

(define (heap-append! heap data)
  (let ((p (heap-proc heap))
        (s (heap-size heap))
        (m (heap-maxsize heap)))
    (if (>= s m)
      (error "heap overflowed!")
      (begin
        (vector-set! (heap-data heap) s data)
        (heap-size-inc! heap))))

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
