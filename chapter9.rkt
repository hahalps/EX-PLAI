#lang plai

(define b (let ([b (box 'dummy)])
            (begin (set-box! b b)
                   b)))

(equal? b (unbox b))

(let ([fact 'dummy])
    (begin
      (set! fact
            (lambda (n)
              (if (zero? n)
                  1
                  (* n (fact (- n 1))))))
      (fact 10)))

; can't use

(define (rec name value body)
  (let ([name 'dummy])
    (set! name value)
    body))

