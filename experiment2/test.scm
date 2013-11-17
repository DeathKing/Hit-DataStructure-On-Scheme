;;; Tests.
;;; 
;;; All tests should be write here.

(load-option 'format)

;;; Binary Tree Test
;;(load "bin-tree++.scm")
(load "thread-tree.scm")

(define t
  (make-tree 'A
    (make-tree 'B (make-leaf 'D)
                  (make-tree 'E (make-leaf 'H) '()))
    (make-tree 'C (make-tree 'F (make-leaf 'I)
                                (make-leaf 'J))
                  (make-tree 'G '() (make-leaf 'K)))))


(define bt (make-tree 'A (make-tree 'B (make-leaf 'D) (make-leaf 'E))
                         (make-tree 'C '() (make-leaf 'F))))

(display "This is how bt shows inner.")
(newline)
(display bt)
(newline)
(display "Now show tree bt in general list.")
(newline)
(tree-general-list-display bt)
(newline)
(display "InOrder Display: ")
(tree-inorder-display bt)
(newline)
(display "PreOrder Display: ")
(tree-preorder-display bt)
(newline)
(display "PostOrder Display: ")
(tree-postorder-display bt)
(newline)
(display "Level Display: ")
(tree-level-display bt)
(newline)
(define bbt (tree-thread bt tree-inorder->list))
(display "The InOrder Thread Binary Tree of bt Should like:")
(newline)
(tree-general-list-display bbt)
(newline)
(format #t "C's next node is: ~A"
        (tree-item (tree-inorder-find-next (tree-right bbt))))
