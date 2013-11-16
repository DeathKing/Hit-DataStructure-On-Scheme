;;; Binary Tree -- More powerful version
;;;
;;; There's a lot of recursion trick here, be careful about that!
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

(define (make-tree item left right)
  (list item left right))

(define (make-leaf item)
  (make-tree item '() '()))

(define (tree-item tree) (list-ref tree 0))
(define (tree-left tree) (list-ref tree 1))
(define (tree-right tree) (list-ref tree 2))

(define (leaf? leaf) (and (null? (tree-left leaf))
                          (null? (tree-right leaf))))

;;; General Stratege for Traversal
;;;
;;; tree -- tree need to traversal
;;; next -- procedure that generate the next node from current node
;;; proc -- how to do with node's data
;;; trav -- the traversal procedure
(define (tree-traversal tree next proc trav)
  (if (null? tree)
    '()
    (next tree proc trav)))

;;; Any traversak can build on tree-traversal
(define (tree-inorder-traversal tree proc trav)
  (trav (tree-left tree) tree-inorder-traversal proc trav)
  (proc (tree-item tree))
  (trav (tree-right tree) tree-inorder-traversal proc trav))

(define (tree-preorder-traversal tree proc trav)
  (proc (tree-item tree))
  (trav (tree-left tree) tree-preorder-traversal proc trav)
  (trav (tree-right tree) tree-preorder-traversal proc trav))

(define (tree-postorder-traversal tree proc trav)
  (trav (tree-left tree) tree-postorder-traversal proc trav)
  (trav (tree-right tree) tree-postorder-traversal proc trav)
  (proc (tree-item tree)))

(define (tree-inorder-display tree)
  (tree-inorder-traversal tree display tree-traversal))

(define (tree-preorder-display tree)
  (tree-preorder-traversal tree display tree-traversal))

(define (tree-postorder-display tree)
  (tree-postorder-traversal tree display tree-traversal))

;;; Level traversal the tree
(define (tree-level-traversal tree proc)
  (do ((queue (cons tree '()) (cdr queue)))
    ((null? queue) '())
    (let ((root (car queue)))
      (if (not (null? root))
        (begin
          (proc (tree-item root))
          (append! queue (list (tree-left root) (tree-right root))))
        '()))))

(define (tree-level-display tree)
  (tree-level-traversal tree display))

;;; Print the tree as a general list -- something like cons cell
;;; FIXED: A atom(leaf) isn't a list
(define (tree-general-list-print tree)
  (cond
    ((null? tree) '())
    ((leaf? tree)
      (display (tree-item tree)))
    (else
      (display (tree-item tree))
      (display "(")
      (tree-general-list-print (tree-left tree))
      (display ",")
      (tree-general-list-print (tree-right tree))
      (display ")"))))

