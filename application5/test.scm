(load-option 'format)

(load "heap-sort.scm")
(load "quick-sort.scm")
(load "radix-sort.scm")
(load "shell-sort.scm")
(load "merge-sort.scm")
(load "bubble-sort.scm")
(load "insert-sort.scm")
(load "select-sort.scm")

(define (make-random-vector size max)
  (make-initialized-vector size
    (lambda (x) (random max))))

(define (make-random-vectors sizes max)
  (vector-map
    (lambda (s) (make-random-vector s max))
    sizes))

(define (test-sort-procedure vecs proc less? report)

  (let ((port (open-output-file report)))
    (vector-map
      (lambda (v)
        ;(format #t "Test vec of size ~A by ~A~%" (vector-length v) proc)
        (with-timings
          (lambda () (proc v less?))
          (lambda (run-time gc-time real-time)
            (format #t "Runtime: ~A GC-Time: ~A Realtime:~A~%" run-time gc-time real-time)
            (format port "~A ~A~%" (vector-length v) (exact->inexact run-time)))))
      vecs)
    (close-output-port port)))

(define svs #(500 1000 1500 2000 2500 3000 3500 4000 4500 5000 5500 6000))
(define svs2 #(500 1000 1500 2000 2500 3000 3500 4000 4500 5000 10000 15000 20000 25000 30000 35000 40000 45000 50000 55000 60000))

;(test-sort-procedure
;  (make-random-vectors svs 100)
;  bubble-sort! < "bubble-sort.data")


(test-sort-procedure
  (make-random-vectors svs2 100)
  merge-sort! < "merge-sort.data")

(test-sort-procedure
  (make-random-vectors svs2 100)
  quick-sort! < "quick-sort.data")

(test-sort-procedure
  (make-random-vectors svs 100)
  heap-sort! < "heap-sort.data")

(let ((port (open-output-file "sort.plt")))
  (format port "set title \"Sort Procedure\"~%")
  (format port "set grid~%")
  (format port "set key right top Left reverse width 0 box 3~%")
  (format port "set xlabel \"Sample Size\"~%")
  (format port "set ylabel \"Sort Time per Element(ticks)\"~%")
  ;(format port "set yrange[0:800]~%")
  (format port "set autoscale y~%")
  (format port "plot \"heap-sort.data\" title \"~A\" with linespoints," "heap-sort")
  (format port "\"bubble-sort.data\" title \"~A\" with linespoints," "bubble-sort")
  (format port "\"quick-sort.data\" title \"~A\" with linespoints," "quick-sort")
  (format port "\"merge-sort.data\" title \"~A\" with linespoints~%" "merge-sort")
  (format port "set terminal png size 640,480~%")
  (format port "set output \"sort.png\"~%")
  (format port "replot~%")
  (close-output-port port))