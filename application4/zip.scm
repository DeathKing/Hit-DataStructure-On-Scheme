(load-option 'format)

(define *default-hash-size* 97)
(define *default-hash-fill* '())

(define (make-hash #!optional size fill)
  (let ((s (if (eq? #!default size) *default-hash-size* size))
        (f (if (eq? #!default fill) *default-hash-fill* fill)))
    (make-vector s f)))

(define (hash-insert! hash proc item)
  (let ((key (modulo (proc item) (vector-length hash))))
    (if (null? (vector-ref hash key))
      (vector-set! hash key (list item))
      (append! (vector-ref hash key) (list item)))))

(define (hash-search hash proc item)
  (let* ((key (modulo (proc item) (vector-length hash)))
         (mem (member item (vector-ref hash key))))
    (if (or (null? mem) (not mem))
      '()
      (car mem))))

(define (hash-delete! hash proc item)
  (let* ((key (proc item))
         (lst (vector-ref hash key)))
    (if (null? list) '()
      (if (= item (car lst))
        (vector-set! hash key (cdr lst))
        (list-delete!/once lst item)))))

(define (mod10 x) (modulo x 10))

(define (hash-pretty-display hash)
  (vector-map
    (lambda (l)
      (if (null? l)
        (format #t "[*EMPTY BUCKET*]~%")
        (begin
          (display (car l))
          (map (lambda (r) (format #t "->~A" r)) (cdr l))
          (newline))))
    hash))

(define h (make-hash 10))

(define v (make-initialized-vector 15 (lambda (x) (random 100))))

(vector-map (lambda (x) (hash-insert! h mod10 x)) v)

(hash-pretty-display h)