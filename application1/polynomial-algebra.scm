;;; SIMPLE POLYNOMIAL ALGEBRA SYSTEM
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(load-option 'format)

;;; TERM Constructor and Selector
(define (make-term var k a) (list 'term var k a))
(define (term-var t) (list-ref t 1))
(define (term-k t)   (list-ref t 2))
(define (term-a t)   (list-ref t 3))

;;; POLY(NOMIAL) Cunstructor and selector
(define (make-poly . z) (append (list 'poly) z))
(define (poly-ref p i)  (list-ref p (- i 1)))
(define (poly-terms p)  (sublist p 1 (length p)))

;;; Predication
(define (monomial? m)      (eq? 'term (list-ref m 0)))
(define (polynomial? p)    (eq? 'poly (list-ref p 0)))

(define (term-like? t1 t2)
  (and (= (term-a t1) (term-a t2))
       (eq? (term-var t1) (term-var t2))))

;;; Useful functions
(define (term-collect t1 t2)
  (if (term-like? t1 t2)
    (make-term (term-var t1) (+ (term-k t1) (term-k t2)) (term-a t1))
    #f))

(define (term-display t . ns)
  (define (term-display-k t ns)
    (begin
      (cond
        ((not (null? ns)) #f)
        ((> (term-k t) 0) (display '+)))
      (cond
        ((= 1 (abs (term-k t))) #f)
	(else
	  (display (term-k t))))))
  (define (term-display-var t)
    (display (term-var t)))
  (define (term-display-a t)
    (if (term-a t) (format #t "^~A" (term-a t))))
  (cond
    ((= 0 (term-k t)) #f)
    (else
      (begin
        (term-display-k t ns)
        (term-display-var t)
        (term-display-a t)))))

(define (poly-display p)
  (if (monomial? p)
    (term-display p)
    (begin
      (term-display (list-ref p 1) #t)
      (map term-display (sublist p 2 (length p))))))

(define (ordered? p) '())

(define (poly-ordering-by p proc)
  (if (monomial? p) p
    (apply make-poly (sort (poly-terms p) proc))))

(define (poly-down-order p)
  (poly-ordering-by p (lambda (x y) (> (term-a x) (term-a y)))))

(define (poly-up-order p)
  (poly-ordering-by p (lambda (x y) (< (term-a x) (term-a y)))))

(define (poly-add p1 p2)
  (define (iter x y z)
    (cond
      ((null? x) (append z y))
      ((null? y) (append z x))
      ((term-like? (car x) (car y))
        (iter (cdr x) (cdr y)
          (append z (list (term-collect (car x) (car y))))))
      ((> (term-a (car x)) (term-a (car y)))
        (iter (cdr x) y (append z (list (car x)))))
      ((< (term-a (car x)) (term-a (car y)))
        (iter x (cdr y) (append z (list (car y)))))
      (else
        (display "mismatched!"))))
  (poly-simplify (apply make-poly
    (iter (if (monomial? p1) (list p1) (cdr p1))
          (if (monomial? p2) (list p2) (cdr p2)) '()))))

(define (poly-sub p1 p2)
  (poly-add p1 (poly-scale p2 -1)))

(define (term-scale t k)
  (make-term (term-var t) (* k (term-k t)) (term-a t)))

(define (poly-scale p k)
  (if (monomial? p)
    (term-scale p k)
    (let ((sub (poly-terms p)))
      (apply make-poly (map term-scale sub (make-list (length sub) k))))))

(define (poly-compact p)
  (if (monomial? p) p
    (apply make-poly
      (filter (lambda (x) (not (= 0 (term-k x)))) (poly-terms p)))))

(define (poly-simplify p)
  (if (monomial? p) p (poly-compact p)))

;test code
(define t1 (make-term 'x  0  0))
(define t2 (make-term 'x  2  1))
(define t3 (make-term 'x  5  3))
(define t4 (make-term 'x -3  4))
(define t5 (make-term 'x -2 -3))

(define p1 (make-poly t3 t5 t4))
(define p2 (make-poly t2 t4 t5))
(define p3 (poly-down-order p1))
(define p4 (poly-down-order p2))

(newline)
(poly-display p3)
(newline)
(poly-display p4)
(newline)
(define p5 (poly-sub p3 p4))
(poly-display p5)