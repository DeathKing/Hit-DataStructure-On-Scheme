;;; Binary Tree -- More powerful version
;;;
;;; There's a lot of recursion trick here, be careful about that!
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

(define (make-tree item left right)
  (list item left right))

(define (make-leaf item)
  (make-tree item '() '()))

(define (tree-item tree)
  (list-ref tree 0))

(define (tree-left tree)
  (list-ref tree 1))

(define (tree-right tree)
  (list-ref tree 2))

;;; General Stratege for Traversal
;;;
;;; tree -- tree need to traversal
;;; next -- procedure that generate the next node from current node
;;; proc -- how to do with node's data
;;; trav -- the traversal procedure
(define (tree-traversal tree next proc trav)
  (if (null? tree)
    '()
    (tree-traversal (next tree proc trav) next proc trav)))

;;; General Inorder Travelsal
(define (tree-inorder-traversal tree proc trav)
  (trav (tree-left tree) tree-inorder-traversal proc trav)
  (proc (tree-item tree))
  (trav (tree-right tree) tree-inorder-traversal proc trav)
  '())

(define (tree-preorder-traversal tree proc trav)
  (proc (tree-item tree))
  (trav (tree-left tree) tree-preorder-traversal proc trav)
  (trav (tree-right tree) tree-preorder-traversal proc trav)
  '())

(define (tree-postorder-traversal tree proc trav)
  (trav (tree-left tree) tree-postorder-traversal proc trav)
  (trav (tree-right tree) tree-postorder-traversal proc trav)
  (proc (tree-item tree))
  '())

(define (tree-inorder-display tree)
  (tree-inorder-traversal tree display tree-traversal))

(define (tree-preorder-display tree)
  (tree-preorder-traversal tree display tree-traversal))

(define (tree-postorder-display tree)
  (tree-postorder-traversal tree display tree-traversal))
 
(define t
  (make-tree 'A
    (make-tree 'B (make-leaf 'D)
                  (make-tree 'E (make-leaf 'H) '()))
    (make-tree 'C (make-tree 'G (make-leaf 'I)
                                (make-leaf 'J))
                  (make-tree 'F '() (make-leaf 'K)))))

(newline)
(tree-inorder-display t)
(newline)
(tree-preorder-display t)
(newline)
(tree-postorder-display t)
(newline)
