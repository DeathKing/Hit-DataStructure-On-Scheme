;;; LIST extension of MIT-Scheme
;;;
;;; Written by: DeathKing<dk@hit.edu.cn>

(define (list-indexp lst element proc)
  (define (iter e l i)
    (cond
      ((null? l) #f)
      ((proc e (car l)) i)
      (else (iter e (cdr l) (+ i 1)))))
  (iter element lst 0))

(define (list-index lst element)
  (list-indexp lst element equal?))

(define (list-memberp? lst element proc)
  (not (list-indexp lst element proc)))

(define (list-member? lst element)
  (list-memberp? lst element equal?))

(define list-include? list-member?)

(define (list-set! l k obj)
  (cond 
    ((= k 0) (set-car! l obj))
    (else
      (list-set! (cdr l) (- k 1) obj))))  
