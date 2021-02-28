#lang racket

(let [(fac 
       (lambda(n)
         (let ([facx (lambda(fac)
                       (lambda(n)
                         (if (= n 0)
                             1
                             (* n ((fac fac) (- n 1))))))])
           ((facx facx) n))))]
  (fac 10))


(let [(fac 
       (lambda(n)
         (let ([fx (lambda(fx)
                    (let ([f (lambda(x) ((fx fx) x))])
                      ((lambda(fac)
                         (lambda(n)
                           (if (= n 0)
                               1
                               (* n (fac (- n 1))))))
                       f)))])
           ((fx fx) n))))]
  (fac 10))

(define mk-rec
  (lambda(body-rec)
    (lambda(n)
      (let ([fx (lambda(fx)
                  (let ([f (lambda(x) ((fx fx) x))])
                    (body-rec f)))])
        ((fx fx) n)))))

(define fib
  (lambda(fibs)
    (lambda(n)
      (if (<= n 1)
          1
          (+ (fibs (- n 1)) (fibs (- n 2)))))))