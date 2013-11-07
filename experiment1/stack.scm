;;;;
;;;; Simple stack implementation.
;;;;
;;;; Written by: DeathKing<dk@hit.edu.cn>

(define (make-stack)
  (cons 'stack '()))

(define (stack? s)
  (cond
    ((null? s) #f)
    ((not (pair? s)) #f)
    (else
      (eq? 'stack (car s)))))

(define (stack s)
  (if (stack? s)
    (cdr s)
    (error "Not a stack. --" stack s)))

(define (stack-size s)
  (length (cdr s)))

(define (stack-empty? s)
  (null? (cdr s)))

(define (stack-print s)
  (display (reverse (stack s))))

(define (stack-top s)
  (if (stack-empty? s)
    (error "Stack is empty. --" stack-top s)
    (car (stack s))))

(define (stack-push! s e)
  (set-cdr! s (cons e (stack s)))
  s)

(define (stack-pop! s)
  (if (stack-empty? s)
    (error "Cannot pop element form a empty stack --" stack-pop! s)
    (let ((ret (car (stack s))))
      (set-cdr! s (cdr (stack s)))
      ret)))

