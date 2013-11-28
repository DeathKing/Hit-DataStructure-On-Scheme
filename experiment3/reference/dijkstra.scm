;; Dijkstra's algorithm
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(load "matrix.scm")
(load "util.scm")

(define (dijkstra1 C)
  
  ; computes the costs of the shortest paths from vertex 0
  ; to every vertex in the cost matrix C
  
  (define n (matrix-col-length C))
  
  (define D (matrix-row-copy C 0))
    ; shortest distance from vertex 0 to each vertex
  
  (define (min-vertex D V)
    ; chooses a vertex w in V such that D[w] is minimum
    (let loop ((w (car V)) (V (cdr V)))
      (if (null? V)
          w
          (if (< (vector-ref D (car V)) (vector-ref D w))
              (loop (car V) (cdr V))
              (loop w (cdr V))))))
  
  ; dijkstra
  (let loop ((V (enum 1 (- n 1))))
    ; V contains the set of vertexes whose shortest
    ; distance from the vertex 0 is still unknown
    (if (null? V)
        D
        (let* ((w (min-vertex D V)) (V-w (remv V w)))
          (for-each
            (lambda (v)
              (vector-set! D v
                (min (vector-ref D v)
                     (+ (vector-ref D w) 
                        (matrix-ref C w v)))))
             V-w)
           (loop V-w)))))

(define (dijkstra2 C)
  
  ; computes a pair of vectors which contain 
  ; (1) the costs of the shortest paths from vertex 0
  ; to every vertex in the cost matrix C and
  ; (2) the immediate predecessor vertexes of every
  ; vertex in the shortest path
  
  (define n (matrix-col-length C))
  
  (define D (matrix-row-copy C 0))
    ; shortest distance from vertex 0 to each vertex
  
  (define P (make-vector n 0))
    ; P[v] contains the vertex immediately before
    ; vertex v in the shortest path
  
  (define (min-vertex D V)
    ; chooses a vertex w in V such that D[w] is minimum
    (let loop ((w (car V)) (V (cdr V)))
      (if (null? V)
          w
          (if (< (vector-ref D (car V)) (vector-ref D w))
              (loop (car V) (cdr V))
              (loop w (cdr V))))))
  
  ; dijkstra
  (let loop ((V (enum 1 (- n 1))))
    ; V contains the set of vertexes whose shortest
    ; distance from the vertex 0 is still unknown
    (if (null? V)
        (cons D P)
        (let* ((w (min-vertex D V)) (V-w (remv V w)))
          (for-each
            (lambda (v)
              (if (< (+ (vector-ref D w) (matrix-ref C w v))
                     (vector-ref D v))
                  (begin
                    (vector-set! D v
                      (+ (vector-ref D w) (matrix-ref C w v))
                    (vector-set! P v w)))))
             V-w)
           (loop V-w)))))

(define (make-cost-matrix size cost-list)
  (define infinity 536870911)
  (define matrix (make-matrix size size infinity))
  (define (set-cost! i j c)
    (matrix-set! matrix i j c))
  (let loop ((i 0))
    (if (< i size)
        (begin
         (matrix-set! matrix i i 0)
         (loop (+ i 1)))))
  (for-each (lambda (lst) (apply set-cost! lst))
            cost-list)
  matrix)

(define C
  (make-cost-matrix 5 
    '((0 1 10) (0 3 30) (0 4 100)
      (1 2 50) (2 4 10) (3 2 20) (3 4 60))))


    
    