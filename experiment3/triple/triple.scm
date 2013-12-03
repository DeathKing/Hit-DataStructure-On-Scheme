(define *triple-version* #(0 0 1))

(define (triple-init)
  (let ((device (make-graphics-device 'x)))
    (if (eq? #f device)
      (error "Cannot make X graphics device.")
      (begin
        (graphics-set-coordinate-limits device -25 -25 25 25)
        device))))


;;; NODE- expect such structure:
;;;   Center:      The center postion of the node
;;;   Radius:      The radius of the node
;;;   Character:   A string or character indecate current
;;;                node. Usualy is a lower letter.
;;;   Proper List: (1 'a)
;;;
;;;   1--2--3
;;;   |/---\|
;;;   4| 5 |6 
;;;   |\___/|
;;;   7--8--9
;;;
;;; Returns a triple node with save the center and radius
(define (triple-draw-node device node)
  (let* ((center (list-ref node 0)) (radius (list-ref node 1))
         (x (car center)) (y (cdr center))
         (s (list-ref node 2)) (p (list-ref node 3)))
    (graphics-operation device 'draw-circle (+ 0.3 x) (+ 0.3 y) radius)
    (graphics-draw-text device x y s)
    (do ((n p (cdr n)))
      ((null? n) '())
      (let ((pos (position-9in1 center radius (caar n))))
        (graphics-draw-text device (car pos) (cdr pos) (cdar n))))
    node))

;;; If theta<n1, n2> < 
(define (triple-connect-node device n1 n2 #!optional style)
  (let* ((c1 (list-ref n1 0)) (c2 (list-ref n2 0))
         (r1 (list-ref n1 1)) (r2 (list-ref n2 1))
         (x1 (car c1)) (y1 (cdr c1)) (x2 (car c2)) (y2 (cdr c2))
         (p1 (cons 0 0)) (p2 (cons 0 0))
         (dx (abs (- x1 x2))) (dy (abs (- y1 y2)))
         (dis (sqrt (+ (* dx dx) (* dy dy)))))
    (cond
      ((<= dis (+ r1 r2)) '()) ;;; 
      ((> dx dy) (if (> x1 x2) (begin (set! p1 (position-9in1 c1 r1 4))
                                      (set! p2 (position-9in1 c2 r2 6)))
                               (begin (set! p1 (position-9in1 c1 r1 6))
                                      (set! p2 (position-9in1 c2 r2 4)))))
      (else (if (> y1 y2) (begin (set! p1 (position-9in1 c1 r1 8))
                                 (set! p2 (position-9in1 c2 r2 2)))
                          (begin (set! p1 (position-9in1 c1 r1 2))
                                 (set! p2 (position-9in1 c2 r2 8))))))
   (if (default-object? style)
     (graphics-draw-line device (car p1) (cdr p1) (car p2) (cdr p2))
     (graphics-bind-line-style device style
        (lambda ()
          (graphics-draw-line device (car p1) (cdr p1) (car p2) (cdr p2)))))))

(define (position-9in1 center radius pos)
  (let ((x (car center)) (y (cdr center)))
    (case pos
      ((1) (cons (- x radius) (+ y radius)))
      ((2) (cons            x (+ y radius)))
      ((3) (cons (+ x radius) (+ y radius)))
      ((4) (cons (- x radius)           y))
      ((5) (cons            x           y))
      ((6) (cons (+ x radius)           y))
      ((7) (cons (- x radius) (- y radius)))
      ((8) (cons            x (- y radius)))
      ((9) (cons (+ x radius) (- y radius)))
      (else (cons           x           y)))))

(define n1 (list (cons 0 0) 1 "1" '()))
(define n2
  (list (cons 3 4) 1 "B"
    (list (cons 3 "5") (cons 9 "#"))))
(define n3
  (list (cons -3 7) 1 "C"
    (list (cons 3 "*"))))

(define d (triple-init))
(triple-draw-node d n1)
(triple-draw-node d n2)
(triple-draw-node d n3)
(triple-connect-node d n1 n2)
(triple-connect-node d n2 n3 2)
(triple-connect-node d n3 n1 7)

