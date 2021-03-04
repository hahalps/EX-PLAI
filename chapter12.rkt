#lang plai-typed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise : Convert the above interpreter to use dynamic scope.;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define f-value (interp f env)]
                        [define a-value (interp a env)])
                  (((closV-f f-value) a-value) env))] 
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [lamC (a b) (closV (lambda(arg-val)
                         (lambda (dy-env)
                           (interp b
                                   (extend-env (bind a arg-val)
                                               dy-env)))))]))


(define-type Value
  [numV (n : number)]
  [closV (f : (Value -> (Env -> Value)))])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;        Another implemenation of Env                ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type-alias Env (symbol -> Value))

(define (mt-env [name : symbol])
  (error 'lookup "name not found"))

(define (extend-env [b : Binding] [e : Env])
  (lambda ([name : symbol]) : Value
    (if (symbol=? name (bind-name b))
        (bind-val b)
        (lookup name e))))

(define (lookup [n : symbol] [e : Env]) : Value
  (e n))

;(define-type-alias Env (listof Binding))
;(define mt-env empty)
;(define extend-env cons)

(define-type Binding
  [bind (name : symbol) (val : Value)])

;(define (lookup [s : symbol] [env : Env]) : Value
;  (cond [(empty? env) (error 'lookup "name not found")]
;        [else (cond [(symbol=? s (bind-name (first env)))
;                     (bind-val (first env))]
;                    [else (lookup s (rest env))])]))

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

;(define-type Value
;  [numV (n : number)]
;  [closV (f : (Value -> Value))])

;(define (interp [expr : ExprC] [env : Env]) : Value
;  (type-case ExprC expr
;    [numC (n) (numV n)]
;    [idC (n) (lookup n env)]
;    [appC (f a) (local ([define f-value (interp f env)]
;                        [define a-value (interp a env)])
;                  ((closV-f f-value) a-value))]
;    [plusC (l r) (num+ (interp l env) (interp r env))]
;    [multC (l r) (num* (interp l env) (interp r env))]
;    [lamC (a b) (closV (lambda (arg-val)
;                         (interp b
;                                (extend-env (bind a arg-val)
;                                             env))))]))

(define (num+ [a : Value] [b : Value]) : Value
  (cond [(and (numV? a) (numV? b))
         (numV (+ (numV-n a) (numV-n b)))]
        [else (error 'num+ "one argument was not a number")]))

(define (num* [a : Value] [b : Value]) : Value
  (cond [(and (numV? a) (numV? b))
         (numV (* (numV-n a) (numV-n b)))]
        [else (error 'num* "one argument was not a number")]))

;; test
(test (interp (plusC (numC 10) (appC (lamC  '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))
 
(test (interp (appC (lamC  'x (appC (lamC  'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          (numV 7))

