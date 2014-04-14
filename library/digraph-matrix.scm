;;; SIMPLE DIAGRAPH 

;;; edge ::= (ui, vi, weight)
;;; vertex :: (data, attribute)

(load "matrix.scm")
(load "matrix-extend.scm")
(load "diagraph-basic.scm")

(define *digraph-unreachable* '())

(define (make-edge ui vi weight)
  (list ui vi weight))

(define (edge-u e) (list-ref e 0))
(define (edge-v e) (list-ref e 1))
(define (edge-wieght) (list-ref e 2))

(define (make-digraph vertexs edges)
  (let* ((s (length vertexs))
  	     (m (make-matrix s s *digraph-unreachable*))
    (do ((e edges (cdr e)))
      ((null? e) '())
      (let ((el (car e)))
      	(matrix-set! m (edge-u e) (edge-v e) (edge-weight e))))
    (cons vertexs m)))

(define (diagraph-edges/list g)
  (let ((r (cons '() '()))
  	    (edges (diagraph-edges g)))
  	(matrix-each-with-index edges
  	  (lambda (data row col)
  	  	(if (eq? data *digraph-unreachable*) '()
  	  	  (append! r (list (make-edge row col data)))))
  	(cdr r)))

