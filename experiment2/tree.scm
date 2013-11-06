; Operation on binary tree 
; We simulate it by using list in Lisp
(define nil '())

(define (type data)
  (car data))

(define (make-leaf data) 
  (list 'leaf data))

(define (make-tree data left right)
  (list 'tree data left right))

(define (atom? x)
  (and (not (null? x))
       (not (list? x))))

(define (tree? tree)
  (cond ((null? tree) #f)
        ((atom? tree) #f)
        (else (eq? (type tree) 'tree))))

(define (leaf? leaf)
  (and (not (tree? leaf))
       (eq? (type leaf) 'leaf)))

(define (tree-data tree)
  (list-ref tree 1))

(define (tree-left tree)
  (if (leaf? tree) nil (list-ref tree 2)))

(define (tree-right tree)
  (if (leaf? tree) nil (list-ref tree 3)))

; General stratege of tree traversal 
(define (tree-traversal tree do-first do-second do-third do-atom)
  (if (null? tree)
    nil
    (if (atom? tree)
      (do-atom tree)
      (if (leaf? tree)
        (do-atom (tree-data tree))
        (begin
          (tree-traversal (do-first tree)  do-first do-second do-third do-atom)
          (tree-traversal (do-second tree) do-first do-second do-third do-atom)
          (tree-traversal (do-third tree)  do-first do-second do-third do-atom))))))

; Preorder tree traversal and print 
(define (preorder-print-tree tree)
  (tree-traversal tree tree-data tree-left tree-right display))

; Inorder tree traversal and print 
(define (inorder-print-tree tree)
  (tree-traversal tree tree-left tree-data tree-right display))

; Postorder tree traversal and print 
(define (postorder-print-tree tree)
  (tree-traversal tree tree-left tree-right tree-data display))

; Layer tree walking
(define (layer-walk tree)
  (define (layer-walk-iter open)
    (if (null? open)
      nil
      (let ((item (car open)))
         (if (null? item)
             (layer-walk-iter (cdr open))
             (let ((left (tree-left item))
                   (right (tree-right item)))
                  (display (tree-data item))
                  (layer-walk-iter (cdr (append open (list left right)))))))))
  (layer-walk-iter (list tree)))

(define (build-from-layer-walk sequence)
  ())

(define l (make-tree 'A 
                     (make-tree 'B nil (make-leaf 'E))
                     (make-tree 'C (make-leaf 'D)
                                   (make-leaf 'F))))
(display l)

(newline)
(preorder-print-tree l)
(newline)
(inorder-print-tree l)
(newline)
(postorder-print-tree l)
(newline)
(layer-walk l)
