(define (graph-vertex g)
  (list-ref g 1))

(define (make-node data . attribute)
  (list 'node data attribute))

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
    ((not (pair? n) #f))
    ((eq? (car n) #t))
    (else #f)))


