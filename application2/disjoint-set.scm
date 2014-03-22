;;; SIMPLE DISJOINT SET IMPLEMENT

(load-option 'format)

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

(define s1 (make-set 1))
(define s2 (make-set 2))
(define s3 (make-set 3))
(define s4 (make-set 4))
(define s5 (make-set 5))
(define s6 (make-set 6))
(define s7 (make-set 7))
(define s8 (make-set 8))

;{1 2 3 5 7}
; {4 6}
; {8}

(set-union! s1 s2)
(set-union! s1 s3)
(set-union! s1 s5)
(set-union! s1 s7)

(set-union! s4 s6)

(format #t "Dose 1R1? >>>~A~%" (set-equal? s1 s1))
(format #t "Dose 1R2? >>>~A~%" (set-equal? s1 s2))
(format #t "Dose 1R3? >>>~A~%" (set-equal? s1 s3))
(format #t "Dose 2R4? >>>~A~%" (set-equal? s2 s4))
(format #t "Dose 4R8? >>>~A~%" (set-equal? s4 s8))
