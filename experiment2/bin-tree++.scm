;;; Binary Tree -- The Ultimate Version
;;;
;;; After trying in bin-tree.scm and bin-tree+.scm,
;;; finally I think this one is pretty powerful.
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

;;; list-set!
;;; Like list-ref, but it set the Kth element in list
(define (list-set! l k obj)
  (cond
    ((= k 0) (set-car! l obj))
    (else
      (list-set! (cdr l) (- k 1) obj))))

;;; The Constructors
(define (make-tree item left right) (list item left right '() '()))
(define (make-leaf item) (list item '() '() '() '()))

;;; The Selectors
(define (tree-item tree) (list-ref tree 0))
(define (tree-left tree) (list-ref tree 1))
(define (tree-right tree) (list-ref tree 2))
(define (tree-ltag tree) (list-ref tree 3))
(define (tree-rtag tree) (list-ref tree 4))
(define (tree-set-left! tree obj) (list-set! tree 1 obj))
(define (tree-set-right! tree obj) (list-set! tree 2 obj))
(define (tree-set-ltag! tree tag) (list-set! tree 3 tag))
(define (tree-set-rtag! tree tag) (list-set! tree 4 tag))

;;; Predication
(define (tree? tree) (and (not (null? tree))
                          (pair? tree)))

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

;;; Following procedure are written with the blief
;;; that we made list as a conventional interface.
;;;
;;; Too much tricks and procedures passing here, you may
;;; get confuse at the first time. But, after all, it's amzing.

(define (tree-inorder->list tree)
  (define (iter t proc trav)
    (append (if (eq? (tree-ltag t) '()) (trav (tree-left t) iter proc trav) '())
            (list t)
            (if (eq? (tree-rtag t) '()) (trav (tree-right t) iter proc trav) '())))
  (iter tree '() tree-traversal))

(define (tree-preorder->list tree)
  (define (iter t proc trav)
    (append (list t)
            (if (eq? (tree-ltag t) '()) (trav (tree-left t) iter proc trav) '())
            (if (eq? (tree-rtag t) '()) (trav (tree-right t) iter proc trav) '())))
  (iter tree '() tree-traversal))

(define (tree-postorder->list tree)
  (define (iter t proc trav)
    (append (if (eq? (tree-ltag t) '()) (trav (tree-left t) iter proc trav) '())
            (if (eq? (tree-rtag t) '()) (trav (tree-right t) iter proc trav) '())
            (list t)))
  (iter tree '() tree-traversal))

;;; Display the tree in a specifical way
(define (tree-inorder-display tree)
  (map (lambda (x) (display (tree-item x)))
    (tree-inorder->list tree)))

(define (tree-preorder-display tree)
  (map (lambda (x) (display (tree-item x)))
    (tree-preorder->list tree)))

(define (tree-postorder-display tree)
  (map (lambda (x) (display (tree-item x)))
    (tree-postorder->list tree)))

(define (tree-general-list-display tree)
  (if (null? tree)
    '()
    (begin
      (display (tree-item tree))
      (display "(")
      (if (eq? (tree-ltag tree) 'thread)
        (if (eq? (tree-ltag (tree-left tree)) 'head)
          (display "[^]")
          (format #t "[~A]" (tree-item (tree-left tree))))
        (tree-general-list-display (tree-left tree)))
      (display ",")
      (if (eq? (tree-rtag tree) 'thread)
        (if (eq? (tree-ltag (tree-right tree)) 'head)
          (display "[^]")
          (format #t "[~A]" (tree-item (tree-right tree))))
        (tree-general-list-display (tree-right tree)))
      (display ")"))))

