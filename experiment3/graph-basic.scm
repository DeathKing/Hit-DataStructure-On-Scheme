;;; GRAGP-BASIC.SCM
;;; This is the basic funcion of graph, it defined some constructors
;;; and selectors.

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
    (else (eq? (car g) 'graph))))

(define (digraph? d)
  (cond
    ((not (pair? d)) #f)
    (else (eq? (car d) 'digraph))))

(define (node? n)
  (cond
    ((not (pair? n)) #f)
    (else (eq? (car n) 'vertex))))


