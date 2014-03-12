;;; SIMPLE STACK IMPLEMENT / VECTOR VERSION
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(define *stack-defualt-size* 128)

(define (make-stack #!optional size object)
  (let ((s (if (eq? size #!default) *stack-defualt-size* size)))
    (list 'stack s 0 (make-vector s object))))

(define (stack? s)
  (cond
    ((null? s) #f)
    ((not (pair? s)) #f)
    (else (eq? 'stack (car s)))))

(define (stack s)
  (if (stack? s)
    (cdr s)
    (error "Not a stack. --" stack s)))
