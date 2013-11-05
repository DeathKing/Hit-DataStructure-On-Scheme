; Operation on binary tree 
; We simulate it by using list in Lisp
(define nil '())

(define (type data)
  (car data))

(define (make-node data) 
  (list 'leaf data))

(define (make-tree data left right)
  (list 'tree data left right))

(define (tree-data tree)
  (list-ref tree 1))

(define (tree-left tree)
  (if (eq? (type tree) 'leaf)
    nil
    (list-ref tree 2)))

(define (tree-right tree)
  (if (eq? (type tree) 'leaf)
    nil
    (list-ref tree 3)))

(define (is-tree? tree)
  (cond ((null? tree) #f)
        ((not (list? tree)) #f)
        (else
          (eq? (car tree) 'tree))))

(define (is-leaf? leaf)
  (and (not (is-tree? leaf))
       (eq? (car leaf) 'leaf)))

(define (is-atom? x)
  (not (list? x)))

;--------------------------------------------------------------

; General stratege of tree walking
(define (walk-tree t do-first do-second do-third do-atom)
  (if (not(null? t))
    (if (is-atom? t)
      (do-atom t)
      (if (is-leaf? t)
        (do-atom (tree-data t))
        (begin (walk-tree (do-first t)  do-first do-second do-third do-atom)
               (walk-tree (do-second t) do-first do-second do-third do-atom)
               (walk-tree (do-third t)  do-first do-second do-third do-atom))))
    nil))

; Preorder tree walking
(define (preorder-print-tree t)
  (walk-tree t tree-data tree-left tree-right display))

; Inorder tree walking
(define (inorder-print-tree t)
  (walk-tree t tree-left tree-data tree-right display))

; Postorder tree walking
(define (postorder-print-tree t)
  (walk-tree t tree-left tree-right tree-data display))

; Layer tree walking
(define (layer-walk tree)
  (define (layer-walk-iter open)
    (if (null? open)
      nil
      (begin
        (let ((item (car open)))
           (if (null? item)
               (layer-walk-iter (cdr open))
               (let ((left (tree-left item))
                     (right (tree-right item)))
                  (display (tree-data item))
                  (layer-walk-iter (cdr (append open (list left right))))))))))
  (layer-walk-iter (list tree)))

(define l (make-tree 'A 
                     (make-tree 'B nil (make-node 'E))
                     (make-tree 'C (make-node 'D)
                                   (make-node 'F))))
(display l)

(newline)
(preorder-print-tree l)
(newline)
(inorder-print-tree l)
(newline)
(postorder-print-tree l)
(newline)
(layer-walk l)
