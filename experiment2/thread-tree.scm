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
(define (tree-set-left! tree obj) (list-set! tree 1 obj))
(define (tree-set-right! tree obj) (list-set! tree 2 obj))
(define (tree-set-ltag! tree tag) (list-set! tree 3 tag))
(define (tree-set-rtag! tree tag) (list-set! tree 4 tag))

(define (tree-inorder-threading tree)
  (define threaded (list tree '() '() 'head 'head))
  
  ;;; t -- 
  ;;; n -- the last traversal node
  (define (iter! t n)
    (if (null? t)
      n
      (let ((n (iter! (tree-left t) n)))
        (if (null? (tree-right n))
          (begin
            (tree-set-right! n t)
            (tree-set-rtag! n 'thread)))
        (if (null? (tree-left t))
          (begin
            (tree-set-left! t n)
            (tree-set-ltag! t 'thread)))
        (iter! (tree-right t) t))))
  
  (iter! (list-ref threaded 0) threaded)
  threaded)

(define (tree-inorder-traversal tree proc trav)
  (if (null? (tree-ltag tree))
    (trav (tree-left tree) tree-inorder-traversal proc trav))
  (proc (tree-item tree))
  (if (null? (tree-rtag tree))
    (trav (tree-right tree) tree-inorder-traversal proc trav)))

(define (tree-inorder-display tree)
  (tree-inorder-traversal tree display tree-traversal))

(define bt (make-tree 'A (make-tree 'B (make-leaf 'C) (make-leaf 'D))
                         (make-tree 'C '() (make-leaf 'E))))

(define (tree-inorder-previous tree)
  (if (eq? '))) 

(newline)
(tree-inorder-display bt)
(newline)
(tree-inorder-threading bt)
(newline)

