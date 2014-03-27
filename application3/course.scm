(define (make-course name)
  (list name 0 '()))

(define (course-name course)
  (list-ref course 0))

(define (course-indegree course)
  (list-ref course 1))

(define (course-depends course)
  (list-ref course 2))

(define (course-set-indegree! course value)
  (set-car! (cdr course) value))

(define (course-inc-indegree! course)
  (course-set-indegree! course (+ (course-indegree course) 1)))

(define (course-dec-indegree! course)
  (course-set-indegree! course (- (course-indegree course) 1)))

(define (course-depend! course depend)
  (course-inc-indegree! course)
  (if (null? (course-depends course))
    (set-car! (cddr depend) (list course))
    (append! (caddr depend) (list course))))

(define (course-finished! course)
  (map course-dec-indegree! (course-depends course)))

(define (course-finished? course)
  (= 0 (course-indegree course)))

(define (course-display course)
  (newline)
  (display (course-name course)))

(define (arrange-course start)
  (let loop ((r (cons '() '()))
             (s start))
    (if (null? s) r
      (let ((ns (cons '() '())))
        (for-each
          (lambda (x)
            (if (course-finished? x)
              (begin
                (course-finished! x)
                (append! r (list x))
                (append! ns (filter course-finished? (course-depends x)))
              '())))
          s)
        (loop r (cdr ns))))
    (cdr r)))

;;; Test Code
(define cpl (make-course "CPL"))
(define asm (make-course "ASM"))
(define compiler (make-course "Compiler"))
(define dis-math (make-course "Discrete Mathematics"))
(define dic-elec (make-course "Digtal Electronics"))
(define com-org (make-course "Computer Orgnization"))
(define os (make-course "Operate System"))

(course-depend! compiler dis-math)
(course-depend! compiler cpl)
(course-depend! asm cpl)
(course-depend! dic-elec dis-math)
(course-depend! com-org dic-elec)
(course-depend! com-org asm)
(course-depend! os com-org)

(define arrange (arrange-course (list cpl dis-math)))

(map course-display arrange)