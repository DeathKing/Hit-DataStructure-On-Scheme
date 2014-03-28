;;; SIMPLE DISJOINT SET IMPLEMENT

(load-option 'format)

(load "disjoint-set.scm")

(define s1 (make-set 1))
(define s2 (make-set 2))
(define s3 (make-set 3))
(define s4 (make-set 4))
(define s5 (make-set 5))
(define s6 (make-set 6))
(define s7 (make-set 7))
(define s8 (make-set 8))

; {1 2 3 5 7}
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
