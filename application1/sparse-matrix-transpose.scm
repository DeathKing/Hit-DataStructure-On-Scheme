;;; SIMPLE SPARSE MATRIX TRANSPOSE
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(load "../library/matrix.scm")
(load-option 'format)

;;; MAKE-SMATRIX: Make sparse matrix
(define (make-smatrix row col #!optional items)
  (list 'smatrix (cons row col)
    (if (eq? #!default items) '() items)))

(define (make-item row col item)
  (list row col item))

(define (smatrix-row s)     (car (list-ref s 1)))
(define (smatrix-col s)     (cdr (list-ref s 1)))
(define (item-row i)        (list-ref i 0))
(define (item-col i)        (list-ref i 1))
(define (item-item i)       (list-ref i 2))

(define (smatrix-row-set! sm v) (set-car! (cadr sm) v))
(define (smatrix-col-set! sm v) (set-cdr! (cadr sm) v))

(define (item-row-set! item v) (set-car! item v))
(define (item-col-set! item v) (set-car! (cdr item) v))

(define (smatrix-items s) (caddr s))
(define (smatrix-count s) (length (smatrix-items s)))

(define (smatrix-add-item! sm item)
  (if (null? (caddr sm))
    (set-car! (cddr sm) (list item))
    (append! (caddr sm) (list item))))

(define (smatrix-transpose s)
  (make-smatrix (smatrix-row s) (smatrix-col s)
    (map (lambda (x) (make-item (item-col x) (item-row x) (item-item x)))
      (smatrix-items s))))

(define (smatrix-transpose! s)
  (display (smatrix-items s))
  (for-each
    (lambda (item)
      (let ((row (item-row item))
            (col (item-col item)))
        (item-row-set! item col)
        (item-col-set! item row)))
    (smatrix-items s))
  (let ((row (smatrix-row s))
        (col (smatrix-col s)))
    (smatrix-row-set! s col)
    (smatrix-col-set! s row)))

(define (smatrix-display sm)
  (let* ((row (smatrix-row sm)) (col (smatrix-col sm))
         (m (make-matrix row col 0)))
    (for-each
      (lambda (x)
        (matrix-set! m (item-row x) (item-col x) (item-item x)))
      (smatrix-items sm))
    (for-each
      (lambda (i)
        (for-each
          (lambda (j) (format #t "~A\t" (matrix-ref m i j)))
          (iota col))
        (newline))
      (iota row))))

; 0 1 0 1
; 0 0 9 0
; 8 0 4 0
; 0 0 0 1
(define s (make-smatrix 4 4))
(newline)
(smatrix-add-item! s (make-item 0 1 1))
(smatrix-add-item! s (make-item 0 1 3))
(smatrix-add-item! s (make-item 1 2 9))
(smatrix-add-item! s (make-item 2 0 8))
(smatrix-add-item! s (make-item 2 2 4))
(smatrix-add-item! s (make-item 3 3 1))
(smatrix-display s)
(smatrix-transpose! s)
(newline)
(newline)
(smatrix-display s)

