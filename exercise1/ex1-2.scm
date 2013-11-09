;;; Find the max and the second max element in a list
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

(define (find-maxs l)
  (define (find-maxs-iter lst ans)
    (if (null? lst)
      ans
      (if (> (car lst) (car ans))
        (find-maxs-iter (cdr lst) (cons (car lst) (car ans)))
        (if (> (car lst) (cdr ans))
          (find-maxs-iter (cdr lst) (cons (car ans) (car lst)))
          (find-maxs-iter (cdr lst) ans)))))
  (find-maxs-iter (cdr l) (cons (car l) (car l))))
