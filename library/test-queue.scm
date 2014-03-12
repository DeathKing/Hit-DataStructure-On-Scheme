(load "queue-list.scm")

(define q (make-queue))

(queue-enqueue! q 1)
(queue-enqueue! q 2)
(queue-enqueue! q 3)
(queue-enqueue! q 4)
(queue-enqueue! q 5)

(queue-dequeue! q)
(display q)
(queue-dequeue! q)
(display q)


