#lang plai-typed
(require "ps4-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps4-ast.rkt and README.md for more information.

;; Note that as in the previous problem set you probably want to implement a version
;; of eval that can return more general values and takes an environment / store as arguments.
;; Your eval-base would then be a wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be converted.
;;
;; For grading, the test cases all result in values that can be converted to base values.

#|
(define-type BaseValue
  [numBV (n : number)]
  [boolBV (b : boolean)])

(define-type Expr
  [numC (n : number)]
  [boolC (b : boolean)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [equal?C (e1 : Expr) (e2 : Expr)]
  [ifC (guard : Expr) (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [beginC (es : (listof Expr))]
  [objectC (delegate : (optionof Expr))
           (fields : (listof (symbol * Expr)))
           (methods : (listof MethodDecl))
           ]
  [msgC (o : Expr) (method : symbol) (args : (listof Expr))]
  [get-fieldC (name : symbol)]
  [set-field!C (name : symbol) (e : Expr)]
  )
|#

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse
                         (second l))
                        (parse (third l)))]
            [(*) (timesC
                  (parse (second l))
                  (parse (third l)))]
            [(equal?) (equal?C
                       (parse (second l))
                       (parse (third l)))]
            [(if) (ifC
                   (parse (second l))
                   (parse (third l))
                   (parse (fourth l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(begin) (beginC (map parse (rest l)))]
            [(object)
             (let* ([fields-se (second l)]
                    [methods-se (third l)]
                    [fields (map (lambda (fd)
                                   (let* ([fdl (s-exp->list fd)]
                                          [fname (s-exp->symbol (first fdl))]
                                          [fexpr (parse (second fdl))])
                                     (pair fname fexpr)))
                                 (s-exp->list fields-se))]
                    [methods (map (lambda (md)
                                    (let* ([mdl (s-exp->list md)]
                                           [mname (s-exp->symbol (first mdl))]
                                           [sig-se (second mdl)]
                                           [args (map s-exp->symbol (s-exp->list sig-se))]
                                           [body (parse (third mdl))])
                                      (method-decl mname args body)))
                                  (s-exp->list methods-se))])
               (objectC (none) fields methods))]
            [(object-del)
             (let* ([delegate-expr (parse (second l))]
                    [fields-se (third l)]
                    [methods-se (fourth l)]
                    [fields (map (lambda (fd)
                                   (let* ([fdl (s-exp->list fd)]
                                          [fname (s-exp->symbol (first fdl))]
                                          [fexpr (parse (second fdl))])
                                     (pair fname fexpr)))
                                 (s-exp->list fields-se))]
                    [methods (map (lambda (md)
                                    (let* ([mdl (s-exp->list md)]
                                           [mname (s-exp->symbol (first mdl))]
                                           [sig-se (second mdl)]
                                           [args (map s-exp->symbol (s-exp->list sig-se))]
                                           [body (parse (third mdl))])
                                      (method-decl mname args body)))
                                  (s-exp->list methods-se))])
               (objectC (some delegate-expr) fields methods))]
            [(msg) (msgC
                    (parse (second l))
                    (s-exp->symbol (third l))
                    (map parse (rest (rest (rest l)))))]
            [(get-field) (get-fieldC (s-exp->symbol (second l)))]
            [(set-field!) (set-field!C (s-exp->symbol (second l)) (parse (third l)))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
         ))]
    )
  )

;; Same definitions as we did in class
(define-type-alias Location number)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define empty-store empty)
(define override-store cons)

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

;; New fetch function for getting the boxed variable from the location in storage
(define (fetch (l : Location) (s : Store)) : Value
  (cond
    [(cons? s)
     (if (equal? (cell-location (first s)) l)
         (cell-val (first s))
         (fetch l (rest s)))]
    [else (error 'lookup "No location found")]))

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))

;; Have a global variable for getting new locations everytime we want to add a new box
;; Similar to how gensym works
;; We are not using gensym directly because we have to change the location as number to symbol
;; which will again conflict with other symbols in the program
(define new-loc
  (let ([counter (box 0)])
    (lambda ()
      (let ([c (unbox counter)])
        (begin (set-box! counter (+ c 1))
               c)))))

;; Define a Value structure which can eventually be connected to BaseValue structure
(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  ;; Closure which refers to lambda expression more or less
  [closV (env : Env) (x : symbol) (e : Expr)]
  ;; Method closure for object methods (captures env, full arg list incl. self, and body)
  [methodV (env : Env) (args : (listof symbol)) (body : Expr)]
  ;; Object: optional delegate value, fields map names to Locations, methods map names to methodV
  [objectV (delegate : (optionof Value))
           (fields : (listof (symbol * Location)))
           (methods : (listof (symbol * Value)))]
  )

;; Helper function to convert Value to BaseValue
(define (value->basevalue (v : Value)) : BaseValue
  (type-case Value v
    [numV (n) (numBV n)]
    [boolV (b) (boolBV b)]
    [else (error 'value->basevalue "Cannot convert this value to a BaseValue")]))

;; We have to return a result instead of a Value
(define-type Result
  [res (v : Value) (s : Store)])

;; We will have a list of Results also defined as a type
(define-type ListResult
  [listres (vs : (listof Value)) (s : Store)])

;; Helper function to evaluate a list of expressions and get the final result
(define (eval-list (env : Env) (sto : Store) (es : (listof Expr))) : ListResult
  (cond
    [(empty? es) (listres empty sto)]
    [(cons? es)
     (type-case Result (eval-env env sto (first es))
       [res (v1 sto1)
            (type-case ListResult (eval-list env sto1 (rest es))
              [listres (vs-rest sto-final)
                       (listres (cons v1 vs-rest) sto-final)])])]))

;; Helper function to check equality between two Values
(define (equal-val? (v1 : Value) (v2 : Value)) : boolean
  (type-case Value v1
    [numV (n1)
          (type-case Value v2
            [numV (n2)
                  (= n1 n2)]
            [else #f])]
    [boolV (b1)
           (type-case Value v2
             [boolV (b2)
                    (equal? b1 b2)]
             [else #f])]
    [else #f]))

;; =======================
;; Association helpers
;; =======================

(define (assoc-field (key : symbol) (alist : (listof (symbol * Location))))
  (cond
    [(empty? alist) (none)]
    [else
     (if (equal? key (fst (first alist)))
         (some (first alist))
         (assoc-field key (rest alist)))]))

(define (assoc-method (key : symbol) (alist : (listof (symbol * Value))))
  (cond
    [(empty? alist) (none)]
    [else
     (if (equal? key (fst (first alist)))
         (some (first alist))
         (assoc-method key (rest alist)))]))

;; Find a field's location following the delegation chain, if necessary
(define (find-field-loc (name : symbol) (obj : Value)) : (optionof Location)
  (type-case Value obj
    [objectV (delegate fields methods)
             (type-case (optionof (symbol * Location)) (assoc-field name fields)
               [some (entry) (some (snd entry))]
               [none ()
                     (type-case (optionof Value) delegate
                       [none () (none)]
                       [some (del-obj) (find-field-loc name del-obj)])])]
    [else (none)]))

;; ==================================
;; Method application with delegation
;; ==================================

(define (bind-args (formals : (listof symbol)) (actuals : (listof Value)) (base-env : Env)) : Env
  (cond
    [(and (empty? formals) (empty? actuals)) base-env]
    [(or (empty? formals) (empty? actuals)) (error 'bind-args "Arity mismatch")]
    [else (bind-args (rest formals) (rest actuals)
                     (extend-env (bind (first formals) (first actuals)) base-env))]))

(define (apply-method (obj : Value) (m : symbol) (args : (listof Value)) (env0 : Env) (sto0 : Store)) : Result
  (type-case Value obj
    [objectV (delegate fields methods)
             (type-case (optionof (symbol * Value)) (assoc-method m methods)
               [some (entry)
                     (let ([meth (snd entry)])
                       (type-case Value meth
                         [methodV (menv margs mbody)
                                  (let ([env* (bind-args margs args menv)])
                                    (eval-env env* sto0 mbody))]
                         [else (error 'apply-method "Malformed method value")]))]
               [none ()
                     (type-case (optionof Value) delegate
                       [none () (error 'apply-method "Unknown method")]
                       [some (del-obj)
                             ;; Delegate search, but keep self as original obj in args
                             (apply-method del-obj m args env0 sto0)])])]
    [else (error 'apply-method "Message to non-object")]))


(define (eval-env (env : Env) (sto : Store) (e : Expr)) : Result
  (type-case Expr e
    [numC (n) (res (numV n) sto)]
    [boolC (b) (res (boolV b) sto)]
    ;; Need to handle the new cases here
    ;; e1 can contain some boxes and set boxes which will change the mutable variable
    ;; and this has to be used in e2 so we have to handle those cases as well
    ;; (+ e1 e2)
    ;; Evaluates e1 and then e2, which can both be assumed to yield numbers n1 and n2, and returns their sum.
    [plusC (e1 e2)
           (type-case Result (eval-env env sto e1)
             [res (v1 sto1)
                  (type-case Result (eval-env env sto1 e2)
                    [res (v2 sto2)
                         (res (numV (+ (numV-n v1)
                                       (numV-n v2)))
                              sto2)])])]
    ;; (* e1 e2)
    ;; Evaluates e1 and then e2, which can both be assumed to yield numbers n1 and n2, and returns their product.
    [timesC (e1 e2)
            (type-case Result (eval-env env sto e1)
              [res (v1 sto1)
                   (type-case Result (eval-env env sto1 e2)
                     [res (v2 sto2)
                          (res (numV (* (numV-n v1)
                                        (numV-n v2)))
                               sto2)])])]
    ;; (equal? e1 e2)
    ;; Evaluates e1 and then e2, and returns true if e1 is equal to e2, and returns false otherwise.
    ;; You can assume that the only values being compared are things that could be represented as BaseValues
    ;; i.e. we will not compare two vectors or two boxes.
    [equal?C (e1 e2)
             (type-case Result (eval-env env sto e1)
               [res (v1 sto1)
                    (type-case Result (eval-env env sto1 e2)
                      [res (v2 sto2)
                           (res (boolV (equal-val? v1 v2)) sto2)])])] ;; Helper function to compare two Values
    ;; (if guard e1 e2)
    ;; Evaluates guard, which can be assumed to yield a boolean b.
    ;; - If b is true, evaluates e1 and returns the result.
    ;; - If b is false, evaluates e2 and returns the result.
    [ifC (g e1 e2)
         (type-case Result (eval-env env sto g)
           [res (vg sto1)
                (if (boolV-b vg)
                    (eval-env env sto1 e1)
                    (eval-env env sto1 e2))])]
    ;; Cheat and use lambda version of let
    ;; (let x e1 e2)
    ;; would be parsed as (letC x e1 e2)
    [letC (x e1 e2)
          (eval-env env sto (appC (lambdaC x e2) e1))]
    ;; (lambda x e)
    [lambdaC (x e) (res (closV env x e) sto)]
    ;; (e1 e2)
    [appC (e1 e2)
          (type-case Result (eval-env env sto e1)
            [res (v1 sto1)
                 (type-case Result (eval-env env sto1 e2)
                   [res (v2 sto2)
                        (eval-env (extend-env (bind (closV-x v1) v2) (closV-env v1))
                                  sto2 ;; Notice store 2 passed here
                                  (closV-e v1))])])]
    [idC (x) (res (lookup x env) sto)]
    ;; (begin e1 e2 e3 ... en)
    ;; evaluates e1, then e2, then e3, ... finally en, and returns the value resulting from evaluating en.
    [beginC (es)
            (cond
              [(empty? es) (error 'eval-env "empty 'begin' is not supported")]
              [(empty? (rest es)) (eval-env env sto (first es))]
              [else
               (type-case Result (eval-env env sto (first es))
                 [res (v1 sto1)
                      (eval-env env sto1 (beginC (rest es)))])])]
    [objectC (delegate fields methods)
             ;; Evaluate delegate (if any), allocate each field into store, build method closures
             (let* ([del-res
                     (type-case (optionof Expr) delegate
                       [none () (res (boolV #f) sto)] ;; placeholder value; we'll convert to (none) below
                       [some (de) (eval-env env sto de)])]
                    [del-val (type-case (optionof Expr) delegate
                               [none () (none)]
                               [some (de) (some (res-v del-res))])]
                    [sto1 (type-case (optionof Expr) delegate
                            [none () sto]
                            [some (de) (res-s del-res)])])
               ;; allocate fields left-to-right
               (local [(define (loop-fields fs acc st)
                         (if (empty? fs)
                             (let ([method-vals
                                    (map (lambda (md)
                                           (pair (method-decl-name md)
                                                 (methodV env (method-decl-args md) (method-decl-body md))))
                                         methods)])
                               (res (objectV del-val acc method-vals) st))
                             (let* ([fname (fst (first fs))]
                                    [fexpr (snd (first fs))]
                                    [fr (eval-env env st fexpr)]
                                    [loc (new-loc)]
                                    [st2 (override-store (cell loc (res-v fr)) (res-s fr))])
                               (loop-fields (rest fs) (cons (pair fname loc) acc) st2))))]
                 (loop-fields fields empty sto1)))]
    ;; (msg o m arg1 ... argN)
    ;; Invokes the method m on object o passing the list of arguments (list arg1 ... argN).
    [msgC (o m args)
          ;; Evaluate receiver, then arguments, then apply method.
          (type-case Result (eval-env env sto o)
            [res (obj sto1)
                 (type-case ListResult (eval-list env sto1 args)
                   [listres (arg-vals sto2)
                            ;; self must be the first formal argument; we pass the receiver object itself.
                            (apply-method obj m (cons obj arg-vals) env sto2)])])]
    ;; Returns the value of the field given by name
    ;; (get-field name)
    [get-fieldC (name)
                (let ([self (lookup 'self env)])
                  (type-case (optionof Location) (find-field-loc name self)
                    [some (loc) (res (fetch loc sto) sto)]
                    [none () (error 'get-fieldC "Unknown field")]))]
    ;; (set-field! name e)
    ;; Evaluates e yielding some value v and then updates the field name to have value v.
    [set-field!C (name e1)
                 (type-case Result (eval-env env sto e1)
                   [res (newv sto1)
                        (let ([self (lookup 'self env)])
                          (type-case (optionof Location) (find-field-loc name self)
                            [some (loc) (res newv (override-store (cell loc newv) sto1))]
                            [none () (error 'set-field!C "Unknown field")]))])]
    #;[else (error 'eval-env "Unimplemeneted")]
    )
  )

(define (eval (e : Expr))
  (eval-env empty-env empty-store e))

(define (eval-base (e : Expr)) : BaseValue
  (type-case Result (eval e)
    [res (v sto)
         (value->basevalue v)]))


