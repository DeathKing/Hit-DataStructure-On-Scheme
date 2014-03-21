;;; SIMPLE DISJOINT SET IMPLEMENT


(define (make-set x)
  (cons x x))

(define (set-father n)
  (cdr n))

(define (set-father-set! n f)
  (set-cdr! n f))

(define (set-data n)
  (car n))

(define (set-find s)
  (let ((f (set-father s)))
    (if (= s f) s (set-find f))))

(define (set-union! x y)
  (let ((xr (set-find x))
        (yr (set-find y)))
    (set-father-set! xr yr)))
  