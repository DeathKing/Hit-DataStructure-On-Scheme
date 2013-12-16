(load "avltree.scm")
(define l (make-avl-tree equal? <))
(define insert-set (list 25 27 30 12 11 18 14 20))
(for-each (lambda (x) (l 'insert! x)) insert-set)

(l 'dot-plot "before-insert-15.dot")
(l 'insert! 15)
(l 'dot-plot "before-insert-22.dot")
(l 'insert! 22)
(l 'dot-plot "after-insert-22.dot")

