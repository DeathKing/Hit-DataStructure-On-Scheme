;;; SIMPLE DISJOINT SET IMPLEMENT

(define (make-set x)
  (cons x x))

(define (set-father n)
  (cdr n))

(define (set-father-set! n f)
  (set-cdr! (set-root-cons n) f))

(define (set-root-cons n)
  (if (number? (cdr n)) n
    (set-root-cons (cdr n))))

(define (set-data n)
  (car n))

(define (set-root s)
  (let ((f (set-father s)))
    (if (number? f) f (set-root f))))

(define (set-union! x y)
  (if (set-equal? x y) '()
    (let ((xr (set-root x))
          (yr (set-root y)))
      (set-father-set! (set-root-cons y) (set-root-cons x)))))

(define (set-equal? x y)
  (= (set-root x) (set-root y)))

