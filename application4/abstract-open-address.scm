(define *default-hash-size* 97)
(define *default-hash-fill* '())
(define *default-hash-delete* 'delete)

(define (make-hash #!optional size fill)
	(let ((s (if (eq? #!default size) *default-hash-size* size))
        (f (if (eq? #!default fill) *default-hash-fill* fill)))
    (make-vector s f)))

(define (hash-search hash proc-key proc-address item)
  (let ((key (proc-key item))
        (length (vector-length hash)))
    (let loop ((t 0))
      (let* ((k (modulo (+ key (proc-address t)) length))
             (x (vector-ref hash k)))
        (if (or (>= t length) (null? x)) '()
          (if (or (eq? *default-hash-delete* x) (not (= x item)))
            (loop (+ t 1))
            x))))))

(define (hash-delete! hash proc-key proc-address item)
  (let ((key (proc-key item))
        (length (vector-length hash)))
    (let loop ((t 0))
      (let* ((k (modulo (+ key (proc-address t)) length))
             (x (vector-ref hash k)))
        (if (or (>= t length) (null? x)) '()
          (if (or (eq? *default-hash-delete* x) (not (= x item)))
            (loop (+ t 1))
            (vector-set! hash k *default-hash-delete*)))))))

(define (hash-insert! hash proc-key proc-address item)
  (let ((key (proc-key item))
        (length (vector-length hash)))
    (let loop ((t 0))
      (let* ((k (modulo (+ key (proc-address t)) length))
             (x (vector-ref hash k)))
        (if (or (>= t length)) (error "hash is full!")
          (if (or (eq? *default-hash-delete* x) (null? x))
            (vector-set! hash k item)
            (loop (+ t 1))))))))


(define (square x) (* x x))

(define (self x) x)

(define (hash-search/linear hash proc item)
  (hash-search hash proc self item))

(define (hash-delete!/linear hash proc item)
  (hash-delete! hash proc self item))

(define (hash-insert!/linear hash proc item)
  (hash-insert! hash proc self item))

(define (hash-search/quadratic hash proc item)
  (hash-search hash proc square item))

(define (hash-delete!/quadratic hash proc item)
  (hash-delete! hash proc square item))

(define (hash-insert!/quadratic hash proc item)
  (hash-insert! hash proc square item))


(define (mod10 x)
  (modulo x 10))

(define v (make-vector 80 0))
(for-each
  (lambda (x) (vector-set! v x (random 100)))
  (iota 80))


(define h1 (make-hash))
(define h2 (make-hash))

(define hl hash-insert!/linear)
(define hq hash-insert!/quadratic)

(map (lambda (x) (hl h1 mod10 x)) (vector->list v))
(map (lambda (x) (hq h2 mod10 x)) (vector->list v))
