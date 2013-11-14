;;; Threaded Binary Tree
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

(load-option 'format)
(load "bin-tree+.scm")

(define (list-set! l k obj)
  (cond
    ((= k 0) (set-car! l obj))
    (else
      (list-set! (cdr l) (- k 1) obj))))

(define (make-tree item left right)
  (list item left right '() '()))

(define (make-leaf item)
  (list item '() '() '() '()))

(define (tree-ltag tree) (list-ref tree 3))
(define (tree-rtag tree) (list-ref tree 4))
(define (tree-set-left! tree obj) (list-set! tree 1 obj))
(define (tree-set-right! tree obj) (list-set! tree 2 obj))
(define (tree-set-ltag! tree tag) (list-set! tree 3 tag))
(define (tree-set-rtag! tree tag) (list-set! tree 4 tag))

(define (tree-inorder-threading tree)
  (define threaded (list tree '() '() 'head 'head))
  ; (display "--:")
  ; (display (list? (car threaded)))
  ; (newline)
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
  
  (let ((l (iter! (list-ref threaded 0) threaded)))
    (if (null? (tree-right l))
      (begin
        (tree-set-right! l threaded)
        (tree-set-rtag! l 'thread))))

  threaded)

(define (tree-inorder-traversal tree proc trav)
  (if (null? (tree-ltag tree))
    (trav (tree-left tree) tree-inorder-traversal proc trav))
  (proc (tree-item tree))
  (if (null? (tree-rtag tree))
    (trav (tree-right tree) tree-inorder-traversal proc trav)))

(define (tree-inorder-display tree)
  (tree-inorder-traversal tree display tree-traversal))

(define (tree-general-list-print tree)
  (define (iter t)
    (if (null? t)
      '()
      (begin
        (display (tree-item t))
        (display "(")
        (if (eq? (tree-ltag t) 'thread)
          (if (eq? (tree-ltag (tree-left t)) 'head)
            (display "[^]")
            (format #t "[~A]" (tree-item (tree-left t))))
          (iter (tree-left t)))
        (display ",")
        (if (eq? (tree-rtag t) 'thread)
          (if (eq? (tree-ltag (tree-right t)) 'head)
            (display "[^]")
            (format #t "[~A]" (tree-item (tree-right t))))
          (iter (tree-right t)))
        (display ")"))))

  (iter (car tree)))

(define bt (make-tree 'A (make-tree 'B (make-leaf 'D) (make-leaf 'E))
                         (make-tree 'C '() (make-leaf 'F))))

(define (tree-inorder-previous tree)
  (define (tree-inorder-last tree)
    ())
  (if (eq? 'thread (tree-rtag tree))
    (tree-right tree)
    (tree-inorder-last (tree-right tree)))) 

(newline)
(define bbt (tree-inorder-threading bt))
(newline)
(tree-general-list-print bbt)

