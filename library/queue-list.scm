;;; SIMPLE QUEUE IMPLEMENT / LIST VERSION
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(define (make-queue)
  (list 'queue (cons '() '())))

(define (queue q)
  (if (queue? q)
    (list-ref q 1)
    (error "Q is not queue.")))

(define (queue? q)
  (cond
    ((null? q) #f)
    ((not (pair? q)) #f)
    (else (eq? 'queue (list-ref q 0)))))

(define (queue-head q)
  (cond
    ((not (queue? q)) (error "Q is not queue."))
    ((queue-empty? q) (error "Q is empty queue."))
    (else             (car (queue q)))))

(define (queue-tail q)
  (cond
    ((not (queue? q)) (error "Q is not queue."))
    ((queue-empty? q) (error "Q is empty queue."))
    (else             (cdr (queue q)))))

(define (queue-empty? q)
  (null? (car (queue q))))

(define (queue-enqueue! q obj)
  (let ((lobj (cons obj '())))
    (if (queue-empty? q)
      (begin
        (set-car! (queue q) lobj)
        (set-cdr! (queue q) lobj))
      (begin
        (set-cdr! (queue-tail q) lobj)
        (set-cdr! (queue q) lobj)))
    q))

(define (queue-dequeue! q)
  (let ((obj (queue-head q)))
    (set-car! (queue q) (cdr obj))
    (car obj)))

(define (queue->list q)
  (car (queue q)))
