#lang plai-typed

(define-type Value
  [numV (n : number)]
  [closV (f : (Value (Value -> Value) -> Value))])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [s : symbol] [env : Env]) : Value
  (cond [(empty? env) (error 'lookup "name not found")]
        [else (cond [(symbol=? s (bind-name (first env)))
                     (bind-val (first env))]
                    [else (lookup s (rest env))])]))

(define (num+ [a : Value] [b : Value]) : Value
  (cond [(and (numV? a) (numV? b))
         (numV (+ (numV-n a) (numV-n b)))]
        [else (error 'num+ "one argument was not a number")]))

(define (num* [a : Value] [b : Value]) : Value
  (cond [(and (numV? a) (numV? b))
         (numV (* (numV-n a) (numV-n b)))]
        [else (error 'num* "one argument was not a number")]))

(define (interp/k [expr : ExprC] [env : Env] [k : (Value -> Value)]) : Value
  (type-case ExprC expr
    [numC (n) (k (numV n))]
    [idC (n) (k (lookup n env))]
    [plusC (l r) (interp/k l env
                           (lambda (lv)
                             (interp/k r env
                                       (lambda (rv)
                                         (k (num+ lv rv))))))]
    [multC (l r) (interp/k l env
                           (lambda (lv)
                             (interp/k r env
                                       (lambda (rv)
                                         (k (num* lv rv))))))]
    [appC (f a) (interp/k f env
                          (lambda (fv)
                            (interp/k a env
                                      (lambda (av)
                                        ((closV-f fv) av k)))))]
    [lamC (a b) (k (closV (lambda (arg-val dyn-k)
                            (interp/k b
                                      (extend-env (bind a arg-val)
                                              env)
                                      dyn-k))))]))

(define (interp [expr : ExprC]) : Value
  (interp/k expr mt-env identity))


;; test
(test (interp (plusC (numC 10) (appC (lamC  '_ (numC 5)) (numC 10))))
      (numV 15))
 
(test (interp (appC (lamC  'x (appC (lamC  'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3)))
          (numV 7))