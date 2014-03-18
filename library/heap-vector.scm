;;; SIMPLE HEAP IMPLEMENT / VECTOR VERSION
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(define *default-heap-size* 64)

(define (make-heap #!optional size)
  (list 'queue 0
    (make-vector (if (eq? #!default size) default-heap-size size) '())))

(define (heap-size h)
  (list-ref h 1))

(define (heap-empty? h)
  (zero? (heap-size h)))

(define (heap-data h)
  (list-ref h 2))

(define (heap-full? h)
  (eq? (+ 1 (heap-size h)) (vector-size (heap-data h))))

