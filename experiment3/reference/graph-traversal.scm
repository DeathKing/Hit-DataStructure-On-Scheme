;;;;
;;;; General graph traversal
;;;;
;;;; Copyright Pertti Kellom\"aki, pk@cs.tut.fi, 1992
;;;; This file may be freely distributed.
;;;;

;;;
;;; A general graph traversal procedure. The arguments are:
;;;   graph:            the graph to be traversed
;;;   starting-vertex   where to start
;;;   insert-vertices   line up a list of vertices to wait for
;;;                     visiting
;;;   next-vertex       get a vertex for visiting or #f if there is none
;;;   visit-proc        procedure to be called for each visited vertex
;;;   fail-proc         procedure to be called for each vertex that is not
;;;                     visited
;;;   postorder?        if absent, do a preorder traversal, otherwise
;;;                     do a postorder traversal

(define (traverse-graph graph starting-vertex
                        insert-vertices next-vertex
                        visit-proc fail-proc . postorder?)

  (let loop ((current starting-vertex)
             (visited '()))

    (if (member current visited)

        ;; we've been here!
        (begin
          (fail-proc current)

          ;; go get a new vertex
          (let ((new (next-vertex)))
            (if new
                (loop new
                      (cons current visited)))))

        ;; a new vertex, handle it
        (begin
          
          ;; visit now?
          (if (not postorder?)
              (visit-proc current))

          ;; line up vertices
          (insert-vertices (vertex-neighbors current graph))

          ;; go get a new vertex
          (let ((new (next-vertex)))
            (if new
                (loop new
                      (cons current visited))))

          ;; visit now?
          (if postorder?
              (visit-proc current))))))


;;;
;;; Breadth first traversal using the above
;;;

(define (bfs graph starting-vertex trace?)

  (define (my-trace . args)
    (if trace?
        (let loop ((args args))
          (cond ((null? args)
                 (newline))
                (else
                 (display (car args))
                 (loop (cdr args)))))))
              
  (let* ((queue (make-queue))
         (insert (lambda (vertices)
                   (map (lambda (v)
                          (my-trace "Insert: " v)
                          (queue 'insert! v))
                        vertices)))
         (next (lambda ()
                 (if (= (queue 'length) 0)
                     #f
                     (let ((vertex (queue 'remove!)))
                       (my-trace "Pick: " vertex)
                       vertex))))
         (visit (lambda (v)
                  (my-trace "Visit:" v)
                  (display v)
                  (newline)))
         (fail (lambda (v)
                 (my-trace "Reject: " v)
                 #f)))

    (traverse-graph graph starting-vertex
                    insert next
                    visit fail)))

;;;
;;; Depth first traversal using the same
;;;

(define (dfs graph starting-vertex trace?)

  (define (my-trace . args)
    (if trace?
        (let loop ((args args))
          (cond ((null? args)
                 (newline))
                (else
                 (display (car args))
                 (loop (cdr args)))))))
              
  (let* ((stack (make-stack))
         (insert (lambda (vertices)
                   (map (lambda (v)
                          (my-trace "Insert: " v)
                          (stack 'push! v))
                        vertices)))
         (next (lambda ()
                 (if (stack 'empty?)
                     #f
                     (let ((vertex (stack 'pop!)))
                       (my-trace "Pick: " vertex)
                       vertex))))
         (visit (lambda (v)
                  (my-trace "Visit:" v)
                  (display v)
                  (newline)))
         (fail (lambda (v)
                 (my-trace "Reject: " v)
                 #f)))

    (traverse-graph graph starting-vertex
                    insert next
                    visit fail)))


;;;
;;; Build a non-trivial graph
;;;

(define g (build-graph '(a b c d e f g h)
                         '((a b)
                           (a d)
                           (b c)
                           (b d)
                           (c d)
                           (c g)
                           (c i)
                           (d e)
                           (e f)
                           (g h)
                           (h i))))
