;;; SIMPLE TREE-BINTREE TRANSFORMER / VECTOR 
;;;
;;; This program can transform bin-tree (data-left-right triple) to
;;; normal tree (data-child-sibling triple) with pretty dot output.
;;;
;;; You can easily draw the picture by using graphviz.
;;;
;;; AUTHOR: DeathKing<dk#hit.edu.cn>
;;; LICENSE: MIT/HIT

(load-option 'format)

(define (make-bintree data left right)
  (let ((v (make-vector 3 '())))
    (vector-set! v 0 data)
    (vector-set! v 1 left)
    (vector-set! v 2 right)
    v))

(define (make-tree data child sibling)
  (let ((v (make-vector 3 '())))
    (vector-set! v 0 data)
    (vector-set! v 1 child)
    (vector-set! v 2 sibling)
    v))

(define (bintree-data bin)  (vector-ref bin 0))
(define (bintree-left bin)  (vector-ref bin 1))
(define (bintree-right bin) (vector-ref bin 2))

(define (tree-data tree)    (vector-ref tree 0))
(define (tree-child tree)   (vector-ref tree 1))
(define (tree-sibling tree) (vector-ref tree 2))

(define (tree-siblings tree)
  (if (null? (tree-sibling tree)) (list tree)
    (let rec ((s (tree-sibling tree))
              (l (list tree)))
      (if (null? s) l
        (rec (tree-sibling s) (append l (list s)))))))

(define (tree-childs tree)
  (if (tree-child-empty? tree) '()
    (tree-siblings (tree-child tree))))

(define (bintree-set-left! bin left)
  (vector-set! bin 1 left))

(define (bintree-set-right! bin right)
  (vector-set! bin 2 right))

(define (tree-set-child! tree child)
  (vector-set! tree 1 child))

(define (tree-set-sibling! tree sibling)
  (vector-set! tree 2 sibling))

(define (bintree-left-empty? bin)
  (let ((l (bintree-left bin)))
    (or (eq? #!default l) (null? l))))

(define (bintree-right-empty? bin)
  (let ((r (bintree-right bin)))
    (or (eq? #!default r) (null? r))))

(define (tree-child-empty? tree)
  (let ((c (tree-child tree)))
    (or (eq? #!default c) (null? c))))

(define (tree-terminal-sibling? tree)
  (let ((s (tree-sibling tree)))
    (or (eq? #!default s) (null? s))))

(define (tree->bintree t)
  (make-bintree (tree-data t)
    (if (tree-child-empty? t) '() (tree->bintree (tree-child t)))
    (if (tree-terminal-sibling? t) '() (tree->bintree (tree-sibling t)))))

(define (bintree->tree b)
  (make-tree (bintree-data b)
    (if (bintree-left-empty? b) '() (bintree->tree (bintree-left b)))
    (if (bintree-right-empty? b) '() (bintree->tree (bintree-right b)))))

(define (tree-dot-plot t file)

  (define (plot node p)
    (map
      (lambda (x)
        (format p "~A -- ~A;~%" (tree-data node) (tree-data x))
        (plot x p))
      (tree-childs node))
    (format p "{ rank=same; ")
    (for-each
      (lambda (x) (format p "~A " (tree-data x)))
      (tree-siblings node))
    (format p "}~%"))

  (let ((port (open-output-file file)))
    (format port "graph tree {~%")
    (for-each (lambda (x) (plot x port)) (tree-siblings t))
    (format port "}")
    (close-output-port port)))

(define (bintree-dot-plot b file)
  (let ((port (open-output-file file)))
    (format port "graph bintree {~%")
    (let polt-rec ((node b))
      (if (bintree-left-empty? node) '()
        (begin (format port "~A:sw -- ~A [splines=line];~%"
          (bintree-data node) (bintree-data (bintree-left node)))
          (polt-rec (bintree-left node))))
      (if (bintree-right-empty? node) '()
        (begin (format port "~A:se -- ~A [splines=line];~%"
          (bintree-data node) (bintree-data (bintree-right node)))
          (polt-rec (bintree-right node)))))
    (format port "}")
    (close-output-port port)))

;;; These are test code
(define b
  (make-bintree 'A
    (make-bintree 'B
      (make-bintree 'E '() '())
      (make-bintree 'C '()
        (make-bintree 'D '() '())))
    (make-bintree 'F
      (make-bintree 'G '() '())
      (make-bintree 'H
        (make-bintree 'I '()
          (make-bintree 'J
            (make-bintree 'K '() '()) '()))
        '())))) 

(define t (bintree->tree b))

(define v (make-tree 'NULL t '()))

(tree-dot-plot    v "tree.dot")
(bintree-dot-plot b "bintree.dot")