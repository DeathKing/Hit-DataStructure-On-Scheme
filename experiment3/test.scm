(load "graph.scm")
(load "string.scm")

(define input (open-input-file "data.in"))
(define n (string->number (read-line input)))
(define v '())
(define e '())

(do ((i n (- i 1)))
  ((= i 0))
  (set! v (append v (list (read-line input)))))

(define m (string->number (read-line input)))

(do ((j m (- j 1)))
  ((= j 0))
  (let ((p (str-split (read-line input) #\space)))
    (set! e (append e (list (cons (string->number (car p))
                                  (string->number (cadr p))))))))
(define a (list 'a 'b 'c 'd 'e 'f 'g))
(define b (list (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4)
                (cons 1 3)
                (cons 2 5) (cons 2 6)
                (cons 3 4)
                (cons 5 6)))

(define g (make-graph (map make-vertex v) e))
(define h (make-graph (map make-vertex a) b))
(define dfst (graph->dfst 0 g))
(define dfst2 (graph->dfst 0 h))
(define bfst (graph->bfst 0 g))

