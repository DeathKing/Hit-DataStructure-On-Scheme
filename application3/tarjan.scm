(load "graph.scm")
(load "../library/stack-list.scm")

(define (tarjan g)
  (define s (make-stack))
  (define vc (graph-vertex-count g))
  (define dfn (make-vector vc 0))
  (define low (make-vector vc '()))
  (define index 1)
  (define pts (make-stack))

  (define (dfs-articul vi)
    (let ((minv index))
      (set! index (+ index 1))
      (vector-set! dfn vi index)
      
            (display dfn)
            (newline)
      (for-each
        (lambda (w)
          (if (null? (vector-ref dfn w))
            (begin
              (dfs-articul w)
              (set! minv (min minv (vector-ref low w)))
              (if (>= (vector-ref low w) (vector-ref dfn vi))
                (begin
                  (stack-push! pts vi)
                  (format #t "Articul Point:~A~%" vi))))
            (set! minv (min minv (vector-ref dfn w)))))
        (vertex-neighbors vi g))
      (vector-set! low vi minv)))

  (define (find-articul u)
    (vector-set! dfn u 1)
    (if (< index vc)
      (begin
        (stack-push! pts u)
        (format #t "Articul Point:~A~%" u)
        (map
          (lambda (x)
            (let ((value (vector-ref dfn x)))
              ;(if (and (not (null? value)) (zero? value))
              (if (zero? value)
                (dfs-articul x))))
          (vertex-neighbors u g)))))

  (find-articul 0)
  pts)


(define lvs
  (list (cons 0 'A) (cons 1 'B) (cons 2 'C) (cons 3 'D) (cons 4 'E) (cons 5 'F) (cons 6 'G)))

(define egs
  (list
    ; a->b        a->c        a->d         a->e
    (list 0 1 6) (list 0 2 1) (list 0 3 5) (list 0 4 5)
    ; b->d        b->e
    (list 1 3 5) (list 1 4 3)
    ; c->f        c->g
    (list 2 5 5) (list 2 6 6)
    ; d->e
    (list 3 4 2)
    ; f->g
    (list 5 6 6)))

(define v (map pair->vertex lvs))
(define e (map list->edge egs))
(define g (make-graph v e))

(tarjan g)