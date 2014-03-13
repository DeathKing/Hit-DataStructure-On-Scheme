;;; str-split : Apr 2006 Doug Hoyte, hcsw.org.
;;; ----
;;; Splits a string 'str into a list of strings
;;; that were separated by the delimiter character 'ch
;;; ----
;;; Efficient as possible given that we can't count on
;;; 'str being an immutable string.
 
(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0))))

; "lineio.scm", line oriented input/output functions for Scheme.
; Copyright (C) 1992, 1993 Aubrey Jaffer
;
(define (read-line . port)
  (let* ((char (apply read-char port)))
    (if (eof-object? char)
      char
    	(do ((char char (apply read-char port))
    	     (clist '() (cons char clist)))
    	  ((or (eof-object? char) (char=? #\newline char))
    	   (list->string (reverse clist)))))))


