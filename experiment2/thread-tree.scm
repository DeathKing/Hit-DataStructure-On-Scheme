;;; Threaded Binary Tree
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

;;; We built threaded tree on binary tree
(load "bin-tree+.scm")

(define (node? node)
  (cond
    ((null? node) #f)
    ((tree? node))))

(define (node-left node)
  (cond
    ((leaf? node) '())
    ((tree? node) (tree-left))
    ((node? node) (tree-left))
    (else '())))


(define (tree-travesal tree do-first do-second do-third do-atom)
  (define thread-tree (list 'thread-tree '() tree tree))
  (define (travesal-iter t tl)
    (cond
      ((null? t) tl)
      ((leaf? t) (begin (do-atom t tl) t))
      (else (begin
              (set! tl (travesal-iter (do-first t) tl))
              (set! tl (travesal-iter (do-second t) tl))
              (set! tl (travesal-iter (do-third t) tl))
              tl))))
  (travesal-iter tree thread-tree)
  thread-tree)

(define (inorder-thread-tree-find-prev tree))

(define (inorder-thread-tree-find-post node))



