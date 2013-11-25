;;; Tests.
;;; 
;;; All tests should be write here.

(load-option 'format)

(load "thread-tree.scm")

(define t
  (make-tree 'A
    (make-tree 'B (make-leaf 'D)
                  (make-tree 'E (make-leaf 'H) '()))
    (make-tree 'C (make-tree 'F (make-leaf 'I)
                                (make-leaf 'J))
                  (make-tree 'G '() (make-leaf 'K)))))


(define bt1 (make-tree 'A (make-tree 'B (make-leaf 'D) (make-leaf 'E))
                          (make-tree 'C '() (make-leaf 'F))))

(define bt2 (make-tree 'A (make-tree 'B (make-leaf 'D) (make-leaf 'E))
                          (make-tree 'C '() (make-leaf 'F))))

(define bt3 (make-tree 'A (make-tree 'B (make-leaf 'D) (make-leaf 'E))
                          (make-tree 'C '() (make-leaf 'F))))


(write-line ";=====================================================")
(write-line ";=====        Binary Tree Test (On Scheme)       =====")
(write-line ";=====================================================")
(newline)
(write-line "------         A tree present in outerenv        -----")
(write-line t)
(newline)
(write-line "------       General list form to present bt     -----")
(tree-general-list-display t)
(newline)
(newline)

(display "[ Recursion]Inorder Display: ")
(tree-inorder-display t)
(newline)
(display "[Interation]Inorder Display: ")
(tree-inorder-display/iter t)
(newline)
(newline)

(display "[ Recursion]Preorder Display: ")
(tree-preorder-display t)
(newline)
(display "[Interation]Preorder Display: ")
(tree-preorder-display/iter t)
(newline)
(newline)

(display "[ Recursion]Postorder Display: ")
(tree-postorder-display t)
(newline)
(display "[Interation]Postorder Display: ")
(tree-postorder-display/iter t)
(newline)
(newline)

(display "[Interation]Level Display: ")
(tree-level-display t)
(newline)
(newline)

(define bbt (tree-thread bt1 tree-inorder->list))

(write-line "-----  General List form of Inorder Thread Tree  -----")
(tree-general-list-display (tree-thread bt1 tree-inorder->list))
(newline)
(newline)

(write-line "----- General List form of Preorder Thread Tree  -----")
(tree-general-list-display (tree-thread bt2 tree-preorder->list))
(newline)
(newline)

(write-line "----- General List form of Postorder Thread Tree -----")
(tree-general-list-display (tree-thread bt3 tree-postorder->list))
(newline)
(newline)
