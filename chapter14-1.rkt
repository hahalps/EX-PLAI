#lang plai-typed

(define-type-alias label number)

(define table (make-hash empty))


(define new-label
  (let ([count 0])
    (lambda()
      (begin (set! count (+ count 1))
             count))))

(define (read-number/suspend [prompt : string] rest)
  (let ([g (new-label)])
    (begin
      (hash-set! table g rest)
      (display prompt)
      (display " To enter it, use the action field label ")
      (display g))))

(define (resume [g : label] [n : number])
  ((some-v (hash-ref table g)) n))

;(read-number/suspend "First number"
;                     (lambda (v1)
;                       (read-number/suspend "Second number"
;                                            (lambda (v2)
;                                              (display
;                                               (+ v1 v2))))))

(define cookie '-100)

(read-number/suspend "First number"
                     (lambda (v1)
                       (begin
                         (set! cookie v1)
                         (read-number/suspend "Second number"
                                            (lambda (v2)
                                              (display
                                               (+ cookie v2)))))))


