;; An AVL tree implementation
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (make-avl-tree equal? less?)
  
  ; memory representation
  (define tree '())
  (define item-count 0)
  
  ; node abstraction
  (define (make-node left item bal right)
    (list item bal left right))
  (define node-left caddr)
  (define node-item car)
  (define node-bal cadr)
  (define node-right cadddr)
  (define (node-set-left! n v)
    (set-car! (cddr n) v))
  (define node-set-item! set-car!)
  (define (node-set-bal! n v)
    (set-car! (cdr n) v))
  (define (node-set-right! n v)
    (set-car! (cdddr n) v))
  
  (define (replace! node parent replacement)
    ; Utility to replace node with replacement
    (if (null? parent)
        (set! tree replacement)
        (if (eq? node (node-left parent))
            (node-set-left! parent replacement)
            (node-set-right! parent replacement))))

  (define (lookup object)
    (let search ((node tree))
      (if (not (null? node))
          (if (equal? object (node-item node))
      	(node-item node)
      	(if (less? object (node-item node))
      	    (search (node-left node))
      	    (search (node-right node))))
          #f)))

  (define (insert! object)
    (let insert ((node tree) (parent '()))
      ; returns #t iff the subtree height has increased
      (cond
        ((null? node)
         (set! item-count (+ item-count 1))
         (let ((new-node (make-node '() object 0 '())))
           (if (not (null? parent))
      	 (if (less? object (node-item parent))
      	     (node-set-left! parent new-node)
      	     (node-set-right! parent new-node))
      	 (set! tree new-node)))
          #t)
        ((less? object (node-item node))
         ; left subtree
         (if (insert (node-left node) node)
             (case (node-bal node)
      	 ((1) (node-set-bal! node 0) #f)
      	 ((0) (node-set-bal! node -1) #t)
      	 ((-1)
      	  (let ((left (node-left node)))
      	    (if (= (node-bal left) -1)
      		(begin  ; single right rotation
      		 (node-set-left! node (node-right left))
      		 (node-set-right! left node)
      		 (node-set-bal! node 0)
      		 (replace! node parent left)
      		 (node-set-bal! left 0))
      		(let ((right (node-right left)))
      		  ; left right rotation
      		  (node-set-right! left (node-left right))
      		  (node-set-left! right left)
      		  (node-set-left! node (node-right right))
      		  (node-set-right! right node)
      		  (if (= (node-bal right) -1)
      		      (node-set-bal! node 1)
      		      (node-set-bal! node 0))
      		  (if (= (node-bal right) 1)
      		      (node-set-bal! left -1)
      		      (node-set-bal! left 0))
      		  (replace! node parent right)
      		  (node-set-bal! right 0))))
      	  #f))
             #f))
        ((not (equal? object (node-item node)))
         ; right subtree
         (if (insert (node-right node) node)
             (case (node-bal node)
      	 ((-1) (node-set-bal! node 0) #f)
      	 ((0) (node-set-bal! node 1) #t)
      	 ((1)
      	  (let ((right (node-right node)))
      	    (if (= (node-bal right) 1)
      		(begin  ; single left rotation
      		 (node-set-right! node (node-left right))
      		 (node-set-left! right node)
      		 (node-set-bal! node 0)
      		 (replace! node parent right)
      		 (node-set-bal! right 0))
      		(let ((left (node-left right)))
      		  ; right left rotation
      		  (node-set-left! right (node-right left))
      		  (node-set-right! left right)
      		  (node-set-right! node (node-left left))
      		  (node-set-left! left node)
      		  (if (= (node-bal left) 1)
      		      (node-set-bal! node -1)
      		      (node-set-bal! node 0))
      		  (if (= (node-bal left) -1)
      		      (node-set-bal! right 1)
      		      (node-set-bal! right 0))
      		  (replace! node parent left)
      		  (node-set-bal! left 0))))
      	  #f))
             #f))
        (else  ; object = (node-item node)
          (node-set-item! node object)
          #f))))
  
  (define (delete! object)
    
    (define (balance1 node parent)
      ; rebalances left subtree and returns #t iff the height of
      ; the subtree has been reduced
      (case (node-bal node)
        ((-1) (node-set-bal! node 0) #t)
        ((0) (node-set-bal! node 1) #f)
        ((1) ; rebalance
         (let* ((right (node-right node)) (right-bal (node-bal right)))
           (if (>= right-bal 0)
      	 (begin ; single left rotate
      	   (node-set-right! node (node-left right))
      	   (node-set-left! right node)
      	   (if (= right-bal 0)
      	       (begin
      		 (node-set-bal! node 1)
      		 (node-set-bal! right -1)
      		 (replace! node parent right)
      		 #f)
      	       (begin
      		 (node-set-bal! node 0)
      		 (node-set-bal! right 0)
      		 (replace! node parent right)
      		 #t)))
      	 (let* ((left (node-left right)) (left-bal (node-bal left)))
      	   ; double right left rotate
      	   (node-set-left! right (node-right left))
      	   (node-set-right! left right)
      	   (node-set-right! node (node-left left))
      	   (node-set-left! left node)
      	   (if (= left-bal 1)
      	       (node-set-bal! node -1)
      	       (node-set-bal! node 0))
      	   (if (= left-bal -1)
      	       (node-set-bal! right 1)
      	       (node-set-bal! right 0))
      	   (replace! node parent left)
      	   (node-set-bal! left 0)
      	   #t))))))
    
    (define (balance2 node parent)
      ; rebalances right subtree and returns #t iff the height of
      ; the subtree has been reduced
      (case (node-bal node)
        ((1) (node-set-bal! node 0) #t)
        ((0) (node-set-bal! node -1) #f)
        ((-1) ; rebalance
         (let* ((left (node-left node)) (left-bal (node-bal left)))
           (if (< left-bal 0)
      	 (begin ; single right rotate
      	   (node-set-left! node (node-right left))
      	   (node-set-right! left node)
      	   (if (= left-bal 0)
      	       (begin
      		 (node-set-bal! node -1)
      		 (node-set-bal! left 1)
      		 (replace! node parent left)
      		 #f)
      	       (begin
      		 (node-set-bal! node 0)
      		 (node-set-bal! left 0)
      		 (replace! node parent left)
      		 #t)))
      	 (let* ((right (node-right left)) (right-bal (node-bal right)))
      	   ; double left right rotate
      	   (node-set-right! left (node-left right))
      	   (node-set-left! right left)
      	   (node-set-left! node (node-right right))
      	   (node-set-right! right node)
      	   (if (= right-bal -1)
      	       (node-set-bal! node 1)
      	       (node-set-bal! node 0))
      	   (if (= right-bal 1)
      	       (node-set-bal! left -1)
      	       (node-set-bal! left 0))
      	   (replace! node parent right)
      	   (node-set-bal! right 0)
      	   #t))))))
    
    (define (replace-smallest! node smallest previous)
      ; Finds the inorder successor of node, deletes it
      ; from previous, and stores its item to node
      (if (null? (node-left smallest))
          (begin  ; smallest was found
            (node-set-item! node (node-item smallest))
            (if (eq? node previous) ; smallest = (node-right node)?
      	  (node-set-right! node (node-right smallest))
      	  (node-set-left! previous (node-right smallest)))
            #t)
          (if (replace-smallest! node (node-left smallest) smallest)
      	(balance1 smallest previous)
      	#f)))
    
    ; delete!
    (let delete ((node tree) (parent '()))
      ; returns #t iff the height of the subtree has been reduced
      (cond
        ((null? node)
         (error "Object is not in avl tree -- DELETE!" object))
        ((less? object (node-item node))
         (if (delete (node-left node) node)
             (balance1 node parent)
             #f))
        ((not (equal? object (node-item node)))
         (if (delete (node-right node) node)
             (balance2 node parent)
             #f))
        (else  ; object = (node-item node)
         (set! item-count (+ item-count 1))
         (cond 
          ((null? (node-right node))
           (replace! node parent (node-left node))
           #t)
          ((null? (node-left node))
           (replace! node parent (node-right node))
           #t)
          (else
           ; node has two children
           (if (replace-smallest! node (node-right node) node)
      	 (balance2 node parent)
      	 #f)))))))

  (define (for-each proc)
    (let repeat ((node tree))
      (if (not (null? node))
          (begin
            (repeat (node-left node))
            (proc (node-item node))
            (repeat (node-right node))))))
  
  (define (dispatch op . args)
    (case op
      ((lookup) (apply lookup args))
      ((insert!) (apply insert! args))
      ((delete!) (apply delete! args))
      ((count) (apply count args))
      ((for-each) (apply for-each args))
      (else (error "Unknown avl tree operation -- DISPATCH" op))))

  dispatch)
