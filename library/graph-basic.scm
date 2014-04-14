(define *edge-unreachable* '())

(define (graph-vertex g) (car g))
(define (graph-edge g)   (cdr g))

(define (make-vertex data #!optional attribute)
  (cons data
  	(if (eq? #!default attribute)
      '()
      attribute)))

(define (pair->vertex p)
  (make-vertex (car p) (cdr p)))

;;; MAKE-EDGE: build new edge
;;;      ui: the index of vertex u
;;;      vi: the index of vertex v
;;;  weight: the weight of the edge, defualt is 1
(define (make-edge ui vi #!optional weight)
  (list ui vi
    (if (eq? #!default weight) *edge-unreachable* weight)))

(define (edge-ui edge)
  (list-ref edge 0))

(define (edge-vi edge)
  (list-ref edge 1))

(define (edge-weight edge)
  (list-ref edge 2))

(define (list->edge lst)
  (apply make-edge lst))

(define (edge-reachable? edge)
  (eq? (edge-weight edge) *edge-weight*))

