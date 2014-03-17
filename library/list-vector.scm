;;; SIMPLE LINEAR LIST IMPLEMENT / VECTOR VERSION
;;;
;;; AUTHOR:  DeathKing<dk#hit.edu.cn>
;;; LICENSE: HIT/MIT

(define *list-default-size* 20)

(define (make-list #!optional size)
  (list 'list 0 (make-vector 
    (if (eq? #!default size) *list-default-size* size))))

(define (list-size lst))

(define (list-insert! lst obj)
  (if (> )))

(define (list-remove! lst index)
  ()
