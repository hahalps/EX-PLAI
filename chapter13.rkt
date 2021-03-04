#lang plai

(define-syntax my-let-1
  (syntax-rules ()
    [(my-let-1 (var val) body)
     (my-let-2 (var val) body)]))


(define-syntax my-let-2
  (syntax-rules ()
    [(my-let-1 (var val) body)
     ((lambda (var) body) val)]))

(test (my-let-1 (x 2) (+ x 1)) 3)

(define-syntax (my-let-3 x)
  (syntax-case x ()
    [(my-let-3 (var val) body)
     (identifier? #'var)
     #'((lambda (var) body) val)]))

; (test/exn (my-let-3 (1 2) 3) "")

(define-syntax (my-or-4 x)
  (syntax-case x ()
    [(my-or-4)
     #'#f]
    [(my-or-4 e)
     #'e]
    [(my-or-4 e0 e1 ...)
     #'(let ([v e0])
         (if v
             v
             (my-or-4 e1 ...)))]))

(test (my-or-4 #f #f #f #t) #t)
(test (let ([init #f])
        (my-or-4 (begin (set! init (not init))
                        init)
               #f))
      #t)

(let ([v #t]) (my-or-4 #f v))

(define-syntax (object/self-3 x)
  (syntax-case x ()
    [(object [mtd-name (var) val] ...)
     (with-syntax ([self (datum->syntax x 'self)])
       #'(let ([self (lambda (msg-name)
                       (lambda (v) (error 'object "nothing here")))])
           (begin
             (set! self
                   (lambda (msg-name)
                     (case msg-name
                       [(mtd-name) (lambda (var) val)]
                       ...)))
             self)))]))


(define os-3
  (object/self-3
   [first (x) (msg self 'second (+ x 1))]
   [second (x) (+ x 1)]))

(define (msg o m . a)
  (apply (o m) a))

(test (msg os-3 'first 1) 3)