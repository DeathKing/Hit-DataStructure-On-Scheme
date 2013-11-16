;;; Threaded Binary Tree
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

(load-option 'format)
(load "bin-tree++.scm")

;;; trav -- A procedure that returns the traversal sequence
;;;         by time.
(define (tree-thread tree trav)
  (define threaded (list tree 'head 'head 'head 'head))
  (define (iter seq n)
    (if (null? seq)
      n
      (let ((c (car seq)))
        (if (null? (tree-left c))
          (begin
            (tree-set-ltag! c 'thread)
            (tree-set-left! c n)))
        (if (null? (tree-right n))
          (begin
            (tree-set-rtag! n 'thread)
            (tree-set-right! n c)))
        (iter (cdr seq) c))))
  (iter (append (trav tree) (list threaded)) threaded)
  (list-ref threaded 0))

(define (tree-inorder-find-next node)
  (if (eq? (tree-rtag node) 'thread)
    (tree-right node)
    (first (tree-inorder->list (tree-right node)))))

(define (tree-preorder-find-next node)
  (if (eq? (tree-rtag node) 'thread)
    (tree-right node)
    (first (tree-preorder->list (tree-left node)))))

