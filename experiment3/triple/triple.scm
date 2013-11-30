(define *triple-version* #(0 0 1))

(define (triple-init)
  (let ((device (make-graphics-device 'x)))
    (if (eq? #f device)
      (error "Cannot make X graphics device.")
      (begin
        (graphics-set-coordinate-limits device -25 -25 25 25)
        device))))


;;; NODE- expect such structure:
;;;   Character:   A String or character indecate current
;;;                node. Usualy is a lower letter.
;;;   Proper List: (1 'a)
;;;
;;;   1--2--3
;;;   |/---\|
;;;   4| 5 |6 
;;;   |\___/|
;;;   7--8--9
(define (triple-draw-node device node center radius)
  (let ((x (car center)) (y (cdr center))
        (s (list-ref node 0)) (p (list-ref node 1)))
    (graphics-operation device 'draw-circle (+ 0.3 x) (+ 0.3 y) radius)
    (graphics-draw-text device x y s)
    (do ((n p (cdr n)))
      ((null? n) '())
      (let ((pos (position-9in1 center radius (caar n))))
        (graphics-draw-text device (car pos) (cdr pos) (cdar n))))))

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

(define n1 (list "1" '()))
(define n2
  (list "b"
    (list (cons 3 "5") (cons 9 "#"))))

(define d (triple-init))
(triple-draw-node d n1 (cons 0 0) 1)
(triple-draw-node d n2 (cons 2 2) 1)
