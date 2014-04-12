(load-option 'format)

(define *default-hash-size* 997)
(define *default-hash-fill* '())
(define *default-hash-delete* 'delete)

(define (make-hash #!optional size fill)
	(let ((s (if (eq? #!default size) *default-hash-size* size))
        (f (if (eq? #!default fill) *default-hash-fill* fill)))
    (make-vector s f)))

(define (hash-search hash proc-key proc-address item)
  (let ((key (proc-key item))
        (length (vector-length hash)))
    (let loop ((t 0))
      (let* ((k (modulo (+ key (proc-address t)) length))
             (x (vector-ref hash k)))
        (if (or (>= t length) (null? x)) '()
          (if (or (eq? *default-hash-delete* x) (not (= x item)))
            (loop (+ t 1))
            x))))))

(define (hash-delete! hash proc-key proc-address item)
  (let ((key (proc-key item))
        (length (vector-length hash)))
    (let loop ((t 0))
      (let* ((k (modulo (+ key (proc-address t)) length))
             (x (vector-ref hash k)))
        (if (or (>= t length) (null? x)) '()
          (if (or (eq? *default-hash-delete* x) (not (= x item)))
            (loop (+ t 1))
            (vector-set! hash k *default-hash-delete*)))))))

(define (hash-insert! hash proc-key proc-address item)
  (let ((key (proc-key item))
        (length (vector-length hash)))
    (let loop ((t 0))
      (let* ((k (modulo (+ key (proc-address t)) length))
             (x (vector-ref hash k)))
        (if (or (>= t length)) (error "hash is full!")
          (if (or (eq? *default-hash-delete* x) (null? x))
            (vector-set! hash k item)
            (loop (+ t 1))))))))

(define (hash-insert!/lots hash proc-key proc-address item-vector)
  (vector-map
    (lambda (x)
      (hash-insert! hash proc-key proc-address x)) item-vector))

(define (hash-search/lots hash proc-key proc-address item-vector)
  (vector-map
    (lambda (x)
      (hash-search hash proc-key proc-address x)) item-vector))

(define (hash-load-factor hash)
  (exact->inexact
    (/ (length (remove null? (vector->list hash)))
       (vector-length hash))))

(define (self x)   x)
(define (square x) (* x x))
(define (cube x)   (* x x x))

(define (hash-search/linear hash proc item)
  (hash-search hash proc self item))

(define (hash-delete!/linear hash proc item)
  (hash-delete! hash proc self item))

(define (hash-insert!/linear hash proc item)
  (hash-insert! hash proc self item))

(define (hash-search/quadratic hash proc item)
  (hash-search hash proc square item))

(define (hash-delete!/quadratic hash proc item)
  (hash-delete! hash proc square item))

(define (hash-insert!/quadratic hash proc item)
  (hash-insert! hash proc square item))

(define (mod10    x) (modulo x 10))
(define (mod11    x) (modulo x 11))
(define (sq-last3 x) (modulo (* x x) 1000))
(define (cb-last3 x) (modulo (* x x x) 100))

(define (repeat times trunk)
  (if (zero? times) '()
    (begin
      (trunk)
      (repeat (- times 1) trunk))))

(define v0 (make-initialized-vector 797 (lambda (x) (random 100))))
(define v1 (make-initialized-vector 697 (lambda (x) (random 100))))
(define v2 (make-initialized-vector 597 (lambda (x) (random 100))))
(define v3 (make-initialized-vector 497 (lambda (x) (random 100))))
(define v4 (make-initialized-vector 397 (lambda (x) (random 100))))

(define t4 (make-initialized-vector 800 (lambda (x) (random 100))))
(define t3 (make-initialized-vector 700 (lambda (x) (random 100))))
(define t2 (make-initialized-vector 600 (lambda (x) (random 100))))
(define t1 (make-initialized-vector 500 (lambda (x) (random 100))))
(define t0 (make-initialized-vector 400 (lambda (x) (random 100))))

; Test sq-last3 in every load factor
(define (test-load-factor file)

  (define th0 (make-hash))  
  (define th1 (make-hash))
  (define th2 (make-hash))
  (define th3 (make-hash))
  (define th4 (make-hash))

  (hash-insert!/lots th0 sq-last3 self v0)
  (hash-insert!/lots th1 sq-last3 self v1)
  (hash-insert!/lots th2 sq-last3 self v2)
  (hash-insert!/lots th3 sq-last3 self v3)
  (hash-insert!/lots th4 sq-last3 self v4)

  (define l0 (hash-load-factor th0))
  (define l1 (hash-load-factor th1))
  (define l2 (hash-load-factor th2))
  (define l3 (hash-load-factor th3))
  (define l4 (hash-load-factor th4))

  (define vs (vector v0 v1 v2 v3 v4))
  (define ts (vector t0 t1 t2 t3 t4))
  (define ls (vector l0 l1 l2 l3 l4))
  (define ss (vector 400 500 600 700 800))

  (for-each

    (lambda (i)
      (let* ((port (open-output-file (format #f "loadfactor~A.data" i)))
             (vec (vector-ref vs i)))

        (for-each
          (lambda (x)
            (let ((scale (vector-ref ss x))
                  (tv (vector-ref ts x)))

              (with-timings
                (lambda () (hash-search/lots vec sq-last3 self tv))
                (lambda (run-time gc-time real-time)
                  (format port "~A ~A~%" scale (exact->inexact (/ real-time scale)))))))

          (iota 5))

          (close-output-port port)))

    (iota 5))

  (let ((port (open-output-file file)))
    (format port "set title \"How LoadFactor affect hash effience\\n(Hash: sq-last3, Addr: self)\"~%")
    (format port "set grid~%")
    (format port "set key left top Left reverse width 0 box 3~%")
    (format port "set xlabel \"Sample Size\"~%")
    (format port "set ylabel \"Search Time per Element(ticks)\"~%")
    ;(format port "set yrange[0:800]~%")
    (format port "set autoscale y~%")
    (format port "plot \"loadfactor0.data\" title \"~A\" with linespoints," l0)
    (format port "\"loadfactor1.data\" title \"~A\" with linespoints," l1)
    (format port "\"loadfactor2.data\" title \"~A\" with linespoints," l2)
    (format port "\"loadfactor3.data\" title \"~A\" with linespoints," l3)
    (format port "\"loadfactor4.data\" title \"~A\" with linespoints~%" l4)
    (format port "set terminal png size 640,480~%")
    (format port "set output \"loadfactor.png\"~%")
    (format port "replot~%")
    (close-output-port port)))

(define (test-key-function file)

  (define th0 (make-hash))  
  (define th1 (make-hash))
  (define th2 (make-hash))

  (hash-insert!/lots th0 mod11 self v0)
  (hash-insert!/lots th1 sq-last3 self v0)
  (hash-insert!/lots th2 cb-last3 self v0)

  (define ts (vector t0 t1 t2 t3 t4))
  (define ss (vector 400 500 600 700 800))
  (define vs (vector th0 th1 th2))
  (define fs (vector mod11 sq-last3 cb-last3))

  (for-each

    (lambda (i)
      (let* ((port (open-output-file (format #f "key~A.data" i)))
             (vec (vector-ref vs i))
             (func (vector-ref fs i)))

        (for-each
          (lambda (x)
            (let ((scale (vector-ref ss x))
                  (tv (vector-ref ts x)))

              (with-timings
                (lambda () (hash-search/lots vec func self tv))
                (lambda (run-time gc-time real-time)
                  (format port "~A ~A~%" scale (exact->inexact (/ real-time scale)))))))

          (iota 5))

          (close-output-port port)))

    (iota 3))


  (let ((port (open-output-file file)))
    (format port "set title \"How key function affect hash effience\\n(LoadFactor: ~A, Addr: self)\"~%" (hash-load-factor th0))
    (format port "set grid~%")
    (format port "set key left top Left reverse width 0 box 3~%")
    (format port "set xlabel \"Sample Size\"~%")
    (format port "set ylabel \"Search Time per Element(ticks)\"~%")
    ;(format port "set yrange[0:800]~%")
    (format port "set autoscale y~%")
    (format port "plot \"key0.data\" title \"~A\" with linespoints," "mod11")
    (format port "\"key1.data\" title \"~A\" with linespoints," "sq-last3")
    (format port "\"key2.data\" title \"~A\" with linespoints~%" "cb-last3")
    (format port "set terminal png size 640,480~%")
    (format port "set output \"key.png\"~%")
    (format port "replot~%")
    (close-output-port port)))

(define (test-addr-function file)

  (define th0 (make-hash))  
  (define th1 (make-hash))
  (define th2 (make-hash))

  (hash-insert!/lots th0 sq-last3 self   v0)
  (hash-insert!/lots th1 sq-last3 square v0)
  (hash-insert!/lots th2 sq-last3 cube   v0)

  (define ts (vector t0 t1 t2 t3 t4))
  (define ss (vector 400 500 600 700 800))
  (define vs (vector th0 th1 th2))
  (define fs (vector self square cube))

  (for-each

    (lambda (i)
      (let* ((port (open-output-file (format #f "addr~A.data" i)))
             (vec (vector-ref vs i))
             (func (vector-ref fs i)))

        (for-each
          (lambda (x)
            (let ((scale (vector-ref ss x))
                  (tv (vector-ref ts x)))

              (with-timings
                (lambda ()
                  (repeat 50 (lambda () (hash-search/lots vec sq-last3 func tv))))
                (lambda (run-time gc-time real-time)
                  (format port "~A ~A~%" scale (exact->inexact (/ real-time scale)))))))

          (iota 5))

          (close-output-port port)))

    (iota 3))

  (let ((port (open-output-file file)))
    (format port "set title \"How addr function affect hash effience\\n(LoadFactor: ~A, Key: sq-last3)\"~%" (hash-load-factor th0))
    (format port "set grid~%")
    (format port "set key left top Left reverse width 0 box 3~%")
    (format port "set xlabel \"Sample Size\"~%")
    (format port "set ylabel \"Search Time per Element(ticks)\"~%")
    ;(format port "set yrange[0:800]~%")
    (format port "set autoscale y~%")
    (format port "plot \"addr0.data\" title \"~A\" with linespoints," "self")
    (format port "\"addr1.data\" title \"~A\" with linespoints," "square")
    (format port "\"addr2.data\" title \"~A\" with linespoints~%" "cube")
    (format port "set terminal png size 640,480~%")
    (format port "set output \"addr.png\"~%")
    (format port "replot~%")
    (close-output-port port)))

(test-addr-function "addr.plt")