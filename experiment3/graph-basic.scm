(define (graph-vertex g)
  (list-ref g 1))

(define (graph-edge g)
  (list-ref g 2))

(define (make-vertex data . attribute)
  (list 'vertex data attribute))

(define (list->vertex lst)
  (make-vertex (list-ref lst 1) (cdr lst)))

(define (graph? g)
  (cond
    ((not (pair? g)) #f)
    ((eq? (car g) 'graph) #t)
    (else #f)))

(define (digraph? d)
  (cond
    ((not (pair? d)) #f)
    ((eq? (car d) 'digraph) #t)
    (else #f)))

(define (node? n)
  (cond
    ((not (pair? n)) #f)
    ((eq? (car n) 'vertex) #t)
    (else #f)))


