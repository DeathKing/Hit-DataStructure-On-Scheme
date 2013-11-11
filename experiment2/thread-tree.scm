;;; Threaded Binary Tree
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

;;; We built threaded tree on binary tree
(load "bin-tree+.scm")

(define (list-set! l k obj)
  (cond
    ((= k 0) (set-car! l obj))
    (else
      (list-set! (cdr l) (- k 1) obj))))

(define (make-tree item left right)
  (list item left right '() '()))

(define (tree-ltag tree) (list-ref tree 3))
(define (tree-rtag tree) (list-ref tree 4))
(define (tree-set-left tree obj) (list-set! tree 1 obj))
(define (tree-set-right tree obj) (list-set! tree 2 obj))
(define (tree-set-ltag tree tag) (list-set! tree 3 tag))
(define (tree-set-rtag tree tag) (list-set! tree 4 tag))

(define (tree-traversal tree proc next prev)
  )

(define (tree-inorder-threading tree)
  (define *prev* (cons '() tree))
  (if (null? (tree-left))
    (begin
      (tree-set-rtag tree 'threaded)
      (tree-set-right tree *prev*)
      (if ())))

