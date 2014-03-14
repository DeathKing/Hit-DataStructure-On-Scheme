;;; SIMPLE BANK INTEREST TABLE
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(load-option 'format)

(define *type:fixed*    'fixed)    ; fixed deposit by installments
(define *type:lump-sum* 'lump-sum) ; lump-sum deposit and withdrawal
(define *type:optional* 'optional) ; time/current optional deposit

(define (make-interest-table) '())

(define (make-interest-item type time rate)
  (list 'item type time rate))

(define (item-type item) (list-ref item 1))
(define (item-tiem item) (list-ref item 2))
(define (item-rate item) (list-ref item 3))

(define (item-fixed?) (eq? *type:fixed* (item-type item)))
(define (item-lump-sum?) (eq? *type:lump-sum* (item-type item)))
(define (item-optional?) (eq? *type:optional* (item-type item)))

(define (format-to-percent f)
  (format #f "~A%" (* 100 f)))

(define (format-time-from-month month)
  (format #f "~A~A"
    (let ((y (/ month 12))) (if (>= y 1) (format #f "~AYear(s)" y) ""))
    (let ((m (modulo month 12))) (if (> m 0) (format #f "~AMonth(s)" m) ""))))

(define (interest-item-display item)
  (format #t "Type: ~A Time: ~A Rate: ~A"
    (item-type item)
    (format-time-from-month (item-time item))
    (format-to-percent (item-rate item))))

(define (interest-table-display tale)
  (map interest-item-display table))

(define (interest-table-insert! table item)
  (append! table item))

(define (interest-table-remove! table index)
  ())