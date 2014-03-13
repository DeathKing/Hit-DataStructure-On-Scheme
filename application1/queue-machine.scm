;;; SIMPLE QUEUE MACHINE
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(load-option 'format)

(define (make-machine)
  ;;; m - the queue
  ;;; i - the number counter
  (define *m* (make-queue))
  (define *i* 0)

  (define (counts)
    (if (queue-empty? *m*)
      0
      (length (car *m*))))

  (define (new)
    (let ((l (counts)))
      (format #t "You're no.~A, and there are ~A customer(s) before you.\n" *i* l)
      (enqueue! *m* *i*)
      (set! *i* (+ *i* 1))))

  (define (empty?)
    (eq? 0 (counts)))

  (define (destory)
    (if (empty?) #f
      (let ((c (dequeue! *m*)))
        (format #t "Customer no.~A, please.\n" c))))

  (define (dispatch op)
    (case op
      ((new) (new))
      ((destory) (destory))
      (else
        (error "No such operation! -- " op))))

  dispatch)

(define m (make-machine))

(let loop ((times 0))
  (if (< (random 100) 65)
    (m 'new)
    (m 'destory))
  (run-shell-command (format #f "sleep ~A" (+ 1 (random 3))))
  (if (< times 50) (loop (+ times 1))))
