(load "graph.scm")

(define v (list 'a 'b 'c 'd 'e 'f 'g))
(define e (list (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4)
                (cons 1 3)
                (cons 2 5) (cons 2 6)
                (cons 3 4)
                (cons 5 6)))

(define g (make-graph (map make-vertex v) e))
(define dfst (graph->dfst 0 g))

