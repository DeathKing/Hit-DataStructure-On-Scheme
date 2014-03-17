;;; SIMPLE BANK INTEREST TABLE
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(load-option 'format)

(define *type:fixed*    'fixed)    ; fixed deposit by installments
(define *type:lump-sum* 'lump-sum) ; lump-sum deposit and withdrawal
(define *type:optional* 'optional) ; time/current optional deposit

(define (make-interest-table)
  (list 'table '()))

(define (make-interest-item type time rate)
  (list 'item type time rate))

(define (interest-terms table)
  (list-ref table 1))

(define (interest-ref table index)
  (list-ref (interest-terms table) index))

(define (item-type item) (list-ref item 1))
(define (item-time item) (list-ref item 2))
(define (item-rate item) (list-ref item 3))

(define (item-fixed?) (eq? *type:fixed* (item-type item)))
(define (item-lump-sum?) (eq? *type:lump-sum* (item-type item)))
(define (item-optional?) (eq? *type:optional* (item-type item)))

(define (format-to-percent f)
  (format #f "~A%" (* 100 f)))

(define (format-time-from-month month)
  (format #f "~A~A"
    (let ((y (/ month 12))) (if (>= y 1) (format #f "~A Year(s)" y) ""))
    (let ((m (modulo month 12))) (if (> m 0) (format #f "~A Month(s)" m) ""))))

(define (interest-item-display item)
  (format #t "\033[33;49;1mType:\033[39;49;0m ~A\t\033[33;49;1mTime:\033[39;49;0m ~A\t\033[33;49;1mRate:\033[39;49;0m ~A\n"
    (item-type item)
    (format-time-from-month (item-time item))
    (format-to-percent (item-rate item))))

(define (interest-table-display-b table)
  (let ((t (interest-terms table)))
    (map interest-item-display
      (sort t
        (lambda (x y) (< (item-time x) (item-time y)))))))

(define (interest-table-display table)
  (map
    (lambda (i)
      (format #t "No.\033[32;49;1m~A\033[39;49;0m\t" i)
      (interest-item-display (interest-ref table i)))
    (iota (length (interest-terms table)))))

(define (interest-table-insert! table item)
  (if (null? (interest-terms table))
    (set-car! (cdr table) (list item))
    (append! (interest-terms table) (list item))))

(define (interest-set! table index item)
  (list-set! (interest-terms table) index item))

;;; data from China Construction Bank
;;; ref: www.ccb.com/cn/personal/interest/rmbdeposit.html
(define t (make-interest-table))

(interest-table-insert! t (make-interest-item *type:fixed*  3 0.0285))
(interest-table-insert! t (make-interest-item *type:fixed*  6 0.0305))
(interest-table-insert! t (make-interest-item *type:fixed* 12 0.0325))
(interest-table-insert! t (make-interest-item *type:fixed* 24 0.0375))
(interest-table-insert! t (make-interest-item *type:fixed* 36 0.0425))
(interest-table-insert! t (make-interest-item *type:fixed* 60 0.0475))

(interest-table-insert! t (make-interest-item *type:lump-sum* 12 0.0285))
(interest-table-insert! t (make-interest-item *type:lump-sum* 36 0.0295))
(interest-table-insert! t (make-interest-item *type:lump-sum* 60 0.0300))

(interest-table-insert! t (make-interest-item *type:optional* 60 0.0300))
(newline)
(interest-table-display t)