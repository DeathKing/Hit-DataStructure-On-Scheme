;;;;
;;;; Representing undirected graphs
;;;;
;;;; Copyright Pertti Kellom\"aki, pk@cs.tut.fi, 1992
;;;; This file may be freely distributed.
;;;;


(define (make-graph vertices edges)
  (list vertices edges))

(define graph-vertices car)
(define graph-edges cadr)

(define (make-vertex name . attributes)
  (list 'vertex name attributes))
(define vertex-name cadr)

(define (make-edge from to . attributes)
  (list 'edge from to attributes))

(define edge-from cadr)
(define edge-to caddr)

(define (vertex-neighbors vertex graph)
  (let loop ((edges (graph-edges graph))
             (result '()))
    (cond ((null? edges)
           (remove-duplicates result))
          ((equal? (edge-from (car edges)) vertex)
           (loop (cdr edges)
                 (cons (edge-to (car edges))
                       result)))
          ((equal? (edge-to (car edges)) vertex)
           (loop (cdr edges)
                 (cons (edge-from (car edges))
                       result)))
          (else
           (loop (cdr edges)
                 result)))))

(define (remove-duplicates lst)
  (cond ((null? lst) '())
        ((member (car lst) (cdr lst))
         (remove-duplicates (cdr lst)))
        (else
         (cons (car lst)
               (remove-duplicates (cdr lst))))))

;;;
;;; Build a graph given the names of its vertices and
;;; the edges as a list of name pairs.
;;;

(define (build-graph vertices edges)
  (make-graph (map make-vertex vertices)
              (map (lambda (edge-description)
                     (make-edge (make-vertex (car edge-description))
                                (make-vertex (cadr edge-description))))
                   edges)))

