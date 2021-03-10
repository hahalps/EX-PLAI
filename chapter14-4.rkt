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

(define-syntax (cps e)
  (syntax-case e (generator with rec lam cnd seq set quote display read-number)
    [(_ (generator (yield) (v) b))
     (and (identifier? #'v) (identifier? #'yield))
     #'(lambda (k)
         (k (let ([where-to-go (lambda (v) (error 'where-to-go "nothing"))])
              (letrec([resumer (lambda (v)
                                 ((cps b) (lambda (k)
                                            (error 'generator "fell through"))))]
                      [yield (lambda (v gen-k)
                               (begin
                                 (set! resumer gen-k)
                                 (where-to-go v)))])
                (lambda (v dyn-k)
                  (begin
                    (set! where-to-go dyn-k)
                    (resumer v)))))))]

    [(_ (with (v e) b))
     #'(cps ((lam (v) b) e))]
    [(_ (rec (v f) b))
     #'(cps (with (v (lam (arg) (error 'dummy "nothing")))
                  (seq
                   (set v f)
                   b)))]
    [(_ (lam (a) b))
     (identifier? #'a)
     #'(lambda (k)
         (k (lambda (a dyn-k)
          ((cps b) dyn-k))))]
    [(_ (cnd tst thn els))
     #'(lambda (k)
         ((cps tst) (lambda (tstv)
                      (if tstv
                          ((cps thn) k)
                          ((cps els) k)))))]
    [(_ (display output))
     #'(lambda (k)
         ((cps output) (lambda (ov)
                         (k (display ov)))))]
    [(_ (read-number prompt))
     #'(lambda (k)
         ((cps prompt) (lambda (pv)
                         (read-number/suspend pv k))))]
    [(_ (seq e1 e2))
     #'(lambda (k)
         ((cps e1) (lambda (_)
                     ((cps e2) k))))]
    [(_ (set v e))
     #'(lambda (k)
         ((cps e) (lambda (ev)
                    (k (set! v ev)))))]
    [(_ 'e)
     #'(lambda (k)
         (k 'e))]
    [(_ (f a))
     #'(lambda (k)
         ((cps f) (lambda (fv)
                    ((cps a) (lambda (av)
                               (fv av k))))))]
    [(_ (f a b))
     #'(lambda (k)
         ((cps a) (lambda (av)
                    ((cps b) (lambda (bv)
                               (k (f av bv)))))))]
    [(_ atomic)
     #'(lambda (k)
         (k atomic))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run c) (c identity ))

(test (run (cps (with (ns (generator (yield) (from)
                                          (rec (f (lam (n)
                                                       (seq
                                                        (yield n)
                                                        (f (+ n 1)))))
                                            (f from))))
                      (ns 0))))
      0)

(test (run (cps (with (ns (generator (yield) (from)
                                          (rec (f (lam (n)
                                                       (seq
                                                        (yield n)
                                                        (f (+ n 1)))))
                                            (f from))))
                      (seq
                       (ns 0)
                       (ns 0)))))
      1)
      
      