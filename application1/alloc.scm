;;; SIMPLE MEMORY ALLOCATION SYSTEM

(define *default-size* 20)

(define (make-record data next) (cons data next))

(define (make-memory)
  (let* ((m (list 0 0 0 (make-vector *default-size* '())))
         (r (list-ref m 3)))
    (for-each
      (lambda (x) (vector-set! r x (make-record '() (+ x 1)))) (iota *default-size*))
    (vector-set! r (- *default-size* 1) (make-record '() -1))
    m))

(define (memory-avali m) (list-ref m 0))
(define (memory-l m)     (list-ref m 1))
(define (memory-m m)     (list-ref m 2))
(define (memory-pool m)  (list-ref m 3))
(define (memory-ref m i) (vector-ref (memory-pool m) i))

(define (memory-avali-record m) (vector-ref (memory-pool m) (memory-avali m)))

(define (record-next r) (cdr r))
(define (record-data r) (car r))
(define (record-next-set! r next) 
  (display r)
  (set-cdr! r next))
(define (record-data-set! r data) (set-car! r data))

(define (memory-alloc! m)
  (let ((p (record-next (memory-avali-record m))))
    (if (= p -1) -1
      (begin
        (record-next-set! (memory-avali-record m) (record-next (memory-ref m p)))
        p))))

(define (memory-free! m q)
  (record-next-set! (memory-ref m q) (record-next (memory-avali-record m)))
  (record-next-set! (memory-avali-record m) q))


;;; Test Code

(define m (make-memory))
(define a (memory-alloc! m))
