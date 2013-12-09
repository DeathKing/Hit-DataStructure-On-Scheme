;;; PLOT.SCM
;;; This file provide the ability to plot the tree structure
;;; using triple, which is a platform free ploting tool.

;;; Load triple support
(load "triple/triple.scm")

(define *canvas* (triple-init))

(define (plot-dfst dfst)
  (define vertex (make-vector (length (list-ref dfst 1)) '()))
  ;;; Plot the node firstly
  (let ((v (list-ref dfsr 1)))
    ))
