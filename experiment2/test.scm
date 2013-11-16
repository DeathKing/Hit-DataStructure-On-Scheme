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

; (newline)
; (tree-inorder-display bt)
; (newline)
; (tree-preorder-display bt)
; (newline)
; (tree-postorder-display bt)
; (newline)
; (tree-general-list-display bt)
; (newline)
; (tree-level-display bt)
; (newline)
(define bbt (tree-thread bt tree-preorder->list))
;(tree-general-list-display bbt)
