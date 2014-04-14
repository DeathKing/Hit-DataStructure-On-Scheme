(define (diagraph-vertexs g) (car g))
(define (diagraph-edges g)   (cdr g))

(define (make-vertex data #!optional attribute)
  (let ((a (if (eq? #!default attribute) '() attribute)))
  	(cons data attribute)))

(define (pair->vertex p)
  (make-vertex (car p) (cdr p)))
