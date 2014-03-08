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
(define (poly-terms-length p)
  (length (poly-terms p)))

;;; Predication
(define (monomial? m)      (eq? 'term (list-ref m 0)))
(define (polynomial? p)    (eq? 'poly (list-ref p 0)))

;;; TERM-LIKE?: check whether t1 and t2 are like-term
;;; t1 - term a
;;; t2 - term b
(define (term-like? t1 t2)
  (and (= (term-a t1) (term-a t2))
       (eq? (term-var t1) (term-var t2))))

;;; Useful functions

;;; TERM-COLLECT: collect the like-term, return #f if t1 and t2 aren't like-term
;;; t1 - term a
;;; t2 - term b
(define (term-collect t1 t2)
  (if (term-like? t1 t2)
    (make-term (term-var t1) (+ (term-k t1) (term-k t2)) (term-a t1)) #f))

;;; TERM-DISPLAY: display a term on the screen
;;;  t - term
;;; ns - whether to show the sign(defualt: yes)
(define (term-display t . ns)

  ;;; TERM_DISPLAY-K: display the coefficient on the screen
  ;;;  t - term
  ;;; ns - whether to show the sign(defualt: yes)
  (define (term-display-k t ns)
    (begin
      (cond
        ((not (null? ns)) #f)
        ((> (term-k t) 0) (display '+)))
      (cond
        ((= 1 (abs (term-k t))) #f)
	(else
	  (display (term-k t))))))

  ;;; TERM-DISPLAY-VAR: display the variable on the screen
  ;;; t - term
  (define (term-display-var t)
    (display (term-var t)))
  
  ;;; TERM-DISPLAY-A: display the power
  ;;; t - term
  (define (term-display-a t)
    (if (term-a t) (format #t "^~A" (term-a t))))

  (cond
    ((= 0 (term-k t)) #f)
    (else (begin
      (term-display-k t ns)
      (term-display-var t)
      (term-display-a t)))))

;;; POLY-DISPLAY: display the polynomial on the screen, the sign woun't be
;;;               shown if it's positive
;;; p - polynomial
(define (poly-display p)
  (if (monomial? p)
    (term-display p #t)
    (begin
      (term-display (list-ref p 1) #t)
      (map term-display (sublist p 2 (length p))))))

;;; POLY-ORDERED?: check whether a polynomial is ordered
;;; p - polynomial
(define (ordered? p) '())

;;; POLY-ORDERING-BY: ordering a polynomial by a proc
;;;    p - polynomial
;;; proc - a two-arguments procedure
(define (poly-ordering-by p proc)
  (if (monomial? p) p
    (apply make-poly (sort (poly-terms p) proc))))

;;; POLY-DOWN-ORDER: down ordering a polynomial
;;; p - polynomial
(define (poly-down-order p)
  (poly-ordering-by p (lambda (x y) (> (term-a x) (term-a y)))))

;;; POLY-DOWN-UP: up ordering a polynomial
;;; p - polynomial
(define (poly-up-order p)
  (poly-ordering-by p (lambda (x y) (< (term-a x) (term-a y)))))

;;; POLY-ADD: polynomial addtion
;;; p1 - polynomial a
;;; p2 - polynomial b
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

;;; POLY-SUB: polynomial subtraction, implement as poly-add
;;; p1 - polynomial a
;;; p2 - polynomial b
(define (poly-sub p1 p2)
  (poly-add p1 (poly-scale p2 -1)))

;;; TERM-MUL: term multiplication
;;; t1 - term a
;;; t2 - term b
(define (term-mul t1 t2)
  (make-term (term-var t1) (* (term-k t1) (term-k t2)) (+ (term-a t1) (term-a t2))))

;;; POLY-MUL: polynomial multiplication
;;; p1 - polynomial a
;;; p2 - polynomial b
(define (poly-mul p1 p2)
  (cond
    ((number? p1) (poly-scale p2 p1))
    ((number? p2) (poly-scale p1 p2))
    ((and (monomial? p1) (monomial? p2)) (term-mul p1 p2))
    ((monomial? p1)
      (poly-simplify (apply make-poly
        (map term-mul (poly-terms p2) (make-list (poly-terms-length p2) p1)))))
    ((monomial? p2)
      (poly-simplify (apply make-poly
        (map term-mul (poly-terms p1) (make-list (poly-terms-length p1) p2)))))
    (else
      (display "not implement yet!"))))

;;; TERM-SCALE: scale a term
;;; t - term
;;; k - the coefficient, must be a const(number? -> #t)
(define (term-scale t k)
  (make-term (term-var t) (* k (term-k t)) (term-a t)))

;;; POLY-SCALE: scale a polynomial
;;; p - polynomial
;;; k - the coefficient, must be a const(number? -> #t)
(define (poly-scale p k)
  (if (monomial? p)
    (term-scale p k)
    (let ((sub (poly-terms p)))
      (apply make-poly (map term-scale sub (make-list (length sub) k))))))

;;; POLY-CAMPACT: remove terms whose coefficient(k) happend to be 0
;;; p - polynomial
(define (poly-compact p)
  (if (monomial? p) p
    (apply make-poly
      (filter (lambda (x) (not (= 0 (term-k x)))) (poly-terms p)))))

;;; POLY-SIMPLIFY: simplify a polynomial
;;; p - polynomial
(define (poly-simplify p var)
  (if (monomial? p) p
    (let ((ht (make-equal-hash-table (length p))))
      (map (lambda (x)
        (let ((value (hash-table/get ht (term-a x) #f)))
          (if value (hash-table/put! ht (term-a x) (+ value (term-k x)))
            (hash-table/put! ht (term-a x) (term-k x)))))
      (poly-terms p))
      (poly-compact (apply make-poly
        (map (lambda (a k) (make-term var k a)) (hash-table/alist ht)))))))


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
;(poly-display p4)
(poly-display t2)
(newline)
;(define p5 (poly-sub p3 p4))
(define p5 (poly-mul p3 t2))
(poly-display p5)
