;;; Hanoi Tower Problem
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

; Enable format procedure. 
(load-option 'format)

(define (hanoi n from to by)
  (if (= n 1)
    (begin 
      (format #t "Move ~A to ~A." from to)
      (newline))
    (begin
      (hanoi (- n 1) from by to)
      (hanoi 1 from to by)
      (hanoi (- n 1) by to from))))

