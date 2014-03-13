;;; SIMPLE SPARSE MATRIX TRANSPOSE
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

;;; MAKE-SMATRIX: Make sparse matrix
(define (make-smatrix row col #!optional items)
  (list 'smatrix (cons row col)
    (if (eq? #!default items) '() items)))

(define (make-item row col item)
  (list row col item))

(define (smatrix-row s)   (car (list-ref s 1)))
(define (smatrix-col s)   (cdr (list-ref s 1)))
(define (item-row i)      (list-ref s 0))
(define (item-col i)      (list-ref s 1))
(define (item-item i)     (iist-ref s 2))

(define (smatrix-items s) (list-ref s 2))
(define (smatrix-count s) (length (smatrix-items s)))

(define (smatrix-add-item! s item)
  (append! (smatrix-items s) (list item)))

(define (smatrix-transpose s)
  (make-smatrix (smatrix-row s) (smatrix-col s)
    (map (lambda (x) (make-item (item-col x) (item-row x) (item-item x)))
      (smatrix-items s))))

(define (smatrix-transpose! s)
  (for-each
    (lambda (item)
      (let ((row (item-row item))
            (col (item-col item)))
        (set! (item-row item) col)
        (set! (item-col item) row)))
    (smatrix-items s)))
