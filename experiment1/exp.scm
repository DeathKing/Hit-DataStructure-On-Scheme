;;; SIMPLE REVERSE POLISH NOTATION EXPRESSION CALCULATOR
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(load "../library/list/stack-list.scm")
(load "../library/string.scm")

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

(define (procedure-priority operator)
  (define *base* 0)
  (cond
    ((eq? operator +) *base*)
    ((eq? operator -) *base*)
    ((eq? operator *) (+ *base* 1))
    ((eq? operator /) (+ *base* 1))
    ((eq? operator expt) (+ *base* 2))
    ((eq? operator modulo) (+ *base* 2))
    ((string=? operator "(") (+ *base* 5))
    (else (- *base* 1))))

;; Does s is a operator?
;; Returns #t only when s is any of + - * / ^ %
(define (string-operator? s)
  (case (string->symbol s)
    ((+ - * / ^ %) #t)
    (else
      (cond
        ((string=? "(" s) #t)
        ((string=? ")" s) #t)
        (else #f)))))

;; Get input and parse to Reverse Polish Expression
;; Returns a expression that contain procedure and number.
(define (input-and-parse)
 
  (define operator-stack (make-stack))
  (define expression (make-stack))

  (let ((s (str-split (read-string (string->char-set "\n")) #\space)))
    (do ((s s (cdr s)))
      ((null? s) expression)
      (let ((e (car s)))
        (if (string-operator? e)
          (cond
            ((string=? "(" e) (stack-push! operator-stack "("))
            ((string=? ")" e)
              (do ()
                ((string=? (stack-top operator-stack) "(")
                 (stack-pop! operator-stack))
                (stack-push! expression (stack-pop! operator-stack))))
            ((stack-empty? operator-stack)
              (stack-push! operator-stack (string-eval-operator e)))
            ((> (procedure-priority (string-eval-operator e))
                (procedure-priority (stack-top operator-stack)))
              (stack-push! operator-stack (string-eval-operator e)))
            (else
              (stack-push! expression (stack-pop! operator-stack))
              (stack-push! operator-stack (string-eval-operator e))))
          (stack-push! expression (string->number e))))))

  (do ()
    ((stack-empty? operator-stack))
    (stack-push! expression (stack-pop! operator-stack)))
  
  (stack-reverse! expression)
  expression)

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

(display (calculate (input-and-parse)))
