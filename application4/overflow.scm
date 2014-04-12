(define *default-hash-size* 97)
(define *default-hash-fill* '())
(define *default-hash-delete* 'delete)

(define (make-hash #!optional size fill)
	(let ((s (if (eq? #!default size) *default-hash-size* size))
        (f (if (eq? #!default fill) *default-hash-fill* fill)))
    (make-vector s f)))