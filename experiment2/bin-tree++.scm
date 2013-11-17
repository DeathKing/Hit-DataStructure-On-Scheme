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

;;; Be compatibel with thread tree.
;;; Returns its original.
(define (tree-real-left tree)
  (if (or (eq? (tree-ltag tree) 'thread)
          (eq? (tree-ltag tree) 'head))
    '()
    (tree-left tree)))

(define (tree-real-right tree)
  (if (or (eq? (tree-rtag tree) 'thread)
          (eq? (tree-rtag tree) 'head))
    '()
    (tree-right tree)))

(define (tree-set-left! tree obj) (list-set! tree 1 obj))
(define (tree-set-right! tree obj) (list-set! tree 2 obj))
(define (tree-set-ltag! tree tag) (list-set! tree 3 tag))
(define (tree-set-rtag! tree tag) (list-set! tree 4 tag))

;;; Predication
(define (tree? tree) (and (not (null? tree))
                          (pair? tree)))

(define (leaf? leaf)
  (or (and (null? (tree-left leaf))
           (null? (tree-right leaf)))
      (and (eq? (tree-ltag leaf) 'thread)
           (eq? (tree-rtag leaf) 'thread))))

;;; Read char form input port and build the tree
(define (build-tree)
  (let ((c (read-char)))
    (if (eq? c #\$)
      '()
      (let* ((left (build-tree))
             (right (build-tree)))
        (make-tree c left right)))))

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
(define (tree-level->list tree)
  (let loop ((s (list tree)) (l '()))
    (if (null? s) l
      (let ((c (car s)))
        (if (not (eq? (tree-ltag c) 'thread))
          (append! s (list (tree-left c))))
        (if (not (eq? (tree-rtag c) 'thread))
          (append! s (list (tree-right c))))
        (loop (cdr s) (append l (list c)))))))

(define (tree-level-display tree)
   (map (lambda (x) (display (tree-item x)))
    (tree-level->list tree)))

;;; Following procedure are written with the blief
;;; that we made list as a conventional interface.
;;;
;;; Too much tricks and procedures passing here, you may
;;; get confuse at the first time. But, after all, it's amzing.

(define (tree-inorder->list tree)
  (define (iter t proc trav)
    (append (trav (tree-real-left t) iter proc trav)
            (proc t)
            (trav (tree-real-right t) iter proc trav)))
  (iter tree list tree-traversal))

(define (tree-preorder->list tree)
  (define (iter t proc trav)
    (append (proc t)
            (trav (tree-real-left t) iter proc trav)
            (trav (tree-real-right t) iter proc trav)))
  (iter tree list tree-traversal))

(define (tree-postorder->list tree)
  (define (iter t proc trav)
    (append (trav (tree-real-left t) iter proc trav)
            (trav (tree-real-right t) iter proc trav)
            (proc t)))
  (iter tree list tree-traversal))

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

