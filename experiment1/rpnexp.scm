;;; Reverse Polish Notation Expression Calculator
;;;
;;; Written by: DeathKing<dk@hit.edu.cn>

(load "stack.scm")

(define (input-and-parse . port)
 
  (define operator-stack (make-stack))
  (define operand-stack (make-stack))
  (define expression '())

  (if (or (null? port)
          (input-port? port))
    (error "Not a valid input port!" input-and-parse port)
    (let ((port (or (if (null? port) #f port)
                    (current-input-port))))
      (let loop ((e (read)))
        (case )
        )
      )
