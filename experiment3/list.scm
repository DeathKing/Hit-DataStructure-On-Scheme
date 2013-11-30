;;; LIST.SCM
;;; Some useful list extension of MIT-Scheme

;;; LIST-INDEXP
;;; Returns the elements index user a procedure proc
;;;
;;; (list-indexp '(1 2 3 4) 4 eq?)              ---> 3 
;;; (list-indexp '("1" "2" "3" "4") "4" eq?)    ---> #f
;;; (list-indexp '("1" "2" "3" "4") "4" equal?) ---> 3
(define (list-indexp lst element proc)
  (define (iter e l i)
    (cond
      ((null? l) #f)
      ((proc e (car l)) i)
      (else (iter e (cdr l) (+ i 1)))))
  (iter element lst 0))

;;; LIST-INDEX
;;; The partial function of list-indexp use equal?
(define (list-index lst element)
  (list-indexp lst element equal?))

;;; LIST-MEMBERP?
;;; Check whether element is in lst by using proc
;;;
;;; (list-memberp? '(1 2 3) 3 eq?)               ---> #t
;;; (list-memberp? '("1" "2" "3") "3" eq?)       ---> #f
;;; (list-memberp? '("1" "2" "3") "3" equal?)    ---> #t
(define (list-memberp? lst element proc)
  (not (list-indexp lst element proc)))

;;; LIST-MEMBER?
;;; The partial function of list-memberp use euqal?
(define (list-member? lst element)
  (list-memberp? lst element equal?))

(define list-include? list-member?)

;;; LIST-SET!
;;; Set the Kth obeject in list l to obj
;;;
;;; (define l '(1 2 4 4))  ---> (1 2 4 4)
;;; (list-set! l 2 3)      ---> Unspecified return value 
;;; l                      ---> (1 2 3 4)
(define (list-set! l k obj)
  (cond 
    ((= k 0) (set-car! l obj))
    (else
      (list-set! (cdr l) (- k 1) obj))))

