;;; Find k in a vector 
;;;
;;; Written: DeathKing<dk@hit.edu.cn>

;;; Find k in a vector
(define (vector-find-k v k)
  (let ((l (vector-length v))
        (i 0))
    (if (= l 0)
      #f
      (let loop ((e (vector-ref v i))
                 (i 0))
        (cond
          ((= e k) #t)
          ((= l (+ i 1)) #f)
          (else
            (loop (vector-ref v (+ i 1)) (+ i 1))))))))

;;; Find k in a list
(define (list-find-k l k)
  (if (null? l)
    #f
    (if (= (car l) k)
      #t
      (list-find-k (cdr l) k))))

