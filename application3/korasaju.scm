(load "diagraph.scm")

(define (korasuju diagraph)
  
  (define time 0)
  (define vc (diagraph-vertex-count diagraph))
  (define color (make-vector vc 'white))
  (define finish (make-vector vc '()))
  (define forest (make-vector vc '()))
  
  (define (dfs g)
    (set! time 0)
    (for-each
      (lambda (vi)
        (if (eq? (vector-ref color vi) 'white)
          (dfs-visit vi g)))
      (map car (diagraph-vertex g))))

  (define (dfs-visit u g)
    (vector-set! color u 'gray)
    (for-each
      (lambda (x)
        (if (eq? (vector-ref color x) 'white)
          (dfs-visit x g)))
      (vertex-neighbors u g))
    (vector-set! color u 'black)
    (vector-set! finish u time)
    (set! time (+ time 1)))

  (define (dfs-mark g)
    (set! color (make-vector vc 'white))
    (let ((sq
      (sort (map car (diagraph-vertex g))
        (lambda (x y)
          (> (vector-ref finish x) (vector-ref finish y))))))
      (let loop ((l sq) (t 0))
        (if (null? l) '()
          (let ((vi (car l)))
            (if (eq? (vector-ref color vi) 'white)
              (dfs-mark-visit vi g t))
            (loop (cdr l) (+ t 1)))))))

  (define (dfs-mark-visit u g t)
    (vector-set! color u 'gray)
    (display t)
        (newline)
    (for-each
      (lambda (x)
        (if (eq? (vector-ref color x) 'white)
          (dfs-mark-visit x g t)))
      (vertex-neighbors u g))
    (vector-set! color u 'black)
    (vector-set! forest u t))

  (dfs diagraph)
  (dfs-mark (diagraph-transpose diagraph))

  forest)


(define lvs
  (list (cons 0 'A) (cons 1 'B) (cons 2 'C) (cons 3 'D) (cons 4 'E) (cons 5 'F)))

(define egs
  (list
    (list 0 1 6) (list 0 2 1)
    (list 1 5 5)
    (list 2 3 5) (list 2 5 4)
    (list 3 0 2)
    (list 4 1 6)
    (list 5 4 6)))

(define v (map pair->vertex lvs))
(define e (map list->edge egs))
(define g (make-diagraph v e))

(diagraph-plot g "org.dot")
(diagraph-plot (diagraph-transpose g) "trans.dot")

(define f (korasuju g))