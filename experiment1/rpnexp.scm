;;; Reverse Polish Notation Expression Calculator
;;;
;;; Written by: DeathKing<dk@hit.edu.cn>

(load "stack.scm")

;; Eval operator to procedure.
;; For many reasons, We didn't use the eval procedure.
(define (string-eval-operator s)
  (case (string->symbol s)
    ((+) +)
    ((-) -)
    ((*) *)
    ((/) /)
    ((^) expt)
    ((%) modulo)))

;; Does s is a operator?
;; Returns #t only when s is any of + - * / ^ %
(define (string-operator? s)
  (case (string->symbol s)
    (( + - * / ^ %) #t)
    (else #f)))

;(define (input-and-parse . port)
; 
;  (define operator-stack (make-stack))
;  (define operand-stack (make-stack))
;  (define expression (make-stack))
;
;  (if (or (null? port)
;          (input-port? port))
;    (error "Not a valid input port!" input-and-parse port)
;    (let ((port (or (if (null? port) #f port)
;                    (current-input-port))))
;      (let loop ((e (read)))
;        (cond
;          ((string=? e "(") (stack-push! s "(")))))))
;;        (case e
  ;;        (("(") (stack-push! operator-stack e))
    ;;      ((")") (let pop-parentheses ((c (stack-pop! s)))
      ;;             (if (string=? c "(")
        ;;             loop
          ;;           pop-parentheses)))
         ;; (("*"))

;; Calculate the reverse polish expression.
;; We use a stack named operand-stack to save the calculate buff.
;; The order of poping the element from operand-stack is **VERY 
;; IMPORTANT**! Be careful about that!
;; The algorithm is easy, but we use some tricks here. 
;;
;; e is a stack which reference to the reverse polish expression.
(define (calculate e)
  (define operand-stack (make-stack))
  (do ((e e e))
    ((stack-empty? e) (stack-top operand-stack))
    (let ((proc (stack-pop! e)))
      (if (procedure? proc)
        (let* ((a (stack-pop! operand-stack))
               (b (stack-pop! operand-stack)))
          (stack-push! operand-stack (proc b a)))
        (stack-push! operand-stack proc))))
  (stack-top operand-stack))

(define s (list->stack (list 2 3 expt 3 expt)))
(stack-reverse! s)
(display s)
(newline)
(display (calculate s))
