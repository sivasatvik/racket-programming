#lang plai-typed

(require "ps2-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;
;; See ps2-ast.rkt and README.md for more information.

;; Env based evaluator preferred - need to avoid dynamic scope
;; Subs based evaluator maybe not - need to avoid capture issue
;; But should be eager evaluator (not lazy)

;; Env based evaluator can use standard library functions easily
;; Subs based cannot, we need to have subs for those library functions as well


#|
(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [listV (vs : (listof Value))])
(define-type Expr
  [valC (v : Value)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [natrecC (e1 : Expr) (e2 : Expr) (x : symbol) (y : symbol) (e3 : Expr)]
  [equal?C (e1 : Expr) (e2 : Expr)]
  [ifC (guard : Expr) (e1 : Expr) (e2 : Expr)]
  [listC (es : (listof Expr))]
  [consC (e1 : Expr) (e2 : Expr)]
  [firstC (e : Expr)]
  [restC (e : Expr)]
  [listrecC (e1 : Expr) (e2 : Expr) (hd : symbol) (rest : symbol) (res : symbol) (e3 : Expr)]
  [letC (bindings : (listof (symbol * Expr))) (e : Expr)]
  [let*C (bindings : (listof (symbol * Expr))) (e : Expr)]
  [unpackC (vars : (listof symbol)) (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  )
|#
(define (parse (s : s-expression)) : Expr
  (cond
    ;; [valC (v : Value)] ----> Value can contain numV or boolV so we parse case by case
    [(s-exp-number? s) (valC (numV (s-exp->number s)))]
    [(s-exp-boolean? s) (valC (boolV (s-exp->boolean s)))]
    ;; [idC (x : symbol)]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            ;; [plusC (e1 : Expr) (e2 : Expr)]
            [(+) (plusC (parse (second l)) (parse (third l)))]
            ;; [timesC (e1 : Expr) (e2 : Expr)]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            ;; [natrecC (e1 : Expr) (e2 : Expr) (x : symbol) (y : symbol) (e3 : Expr)]
            ;; Usage: (natrec e1 e2 (x y e3))
            [(natrec) (natrecC
                       (parse (second l))
                       (parse (third l))
                       (s-exp->symbol (first (s-exp->list (fourth l))))
                       (s-exp->symbol (second (s-exp->list (fourth l))))
                       (parse (third (s-exp->list (fourth l)))))]
            ;; [equal?C (e1 : Expr) (e2 : Expr)]
            [(equal?) (equal?C
                       (parse (second l))
                       (parse (third l)))]
            ;; [ifC (guard : Expr) (e1 : Expr) (e2 : Expr)]
            [(if) (ifC
                   (parse (second l))
                   (parse (third l))
                   (parse (fourth l)))]
            ;; [listC (es : (listof Expr))]
            [(list) (listC
                     (map parse (rest l)))]
            ;; [consC (e1 : Expr) (e2 : Expr)]
            [(cons) (consC
                     (parse (second l))
                     (parse (third l)))]
            ;; [firstC (e : Expr)]
            [(first) (firstC
                      (parse (second l)))]
            ;; [restC (e : Expr)]
            [(rest) (restC
                     (parse (second l)))]
            ;; [listrecC (e1 : Expr) (e2 : Expr) (hd : symbol) (rest : symbol) (res : symbol) (e3 : Expr)]
            ;; Usage: (listrec e1 e2 (hd rest res e3))
            [(listrec) (listrecC
                        (parse (second l))
                        (parse (third l))
                        (s-exp->symbol (first (s-exp->list (fourth l))))
                        (s-exp->symbol (second (s-exp->list (fourth l))))
                        (s-exp->symbol (third (s-exp->list (fourth l))))
                        (parse (fourth (s-exp->list (fourth l)))))]
            ;; [letC (bindings : (listof (symbol * Expr))) (e : Expr)]
            ;; Usage: (let ([x1 e1] [x2 e2] ... [xn en]) e)
            [(let) (letC
                    (map
                     (lambda (b)
                       (let ([pair-list (s-exp->list b)])
                         (pair (s-exp->symbol (first pair-list))
                               (parse (second pair-list)))))
                     (s-exp->list (second l)))
                    (parse (third l)))]
            ;; [let*C (bindings : (listof (symbol * Expr))) (e : Expr)]
            ;; Usage: (let* ([x1 e1] [x2 e2] ... [xn en]) e)
            [(let*) (let*C
                    (map
                     (lambda (b)
                       (let ([pair-list (s-exp->list b)])
                         (pair (s-exp->symbol (first pair-list))
                               (parse (second pair-list)))))
                     (s-exp->list (second l)))
                    (parse (third l)))]
            ;; [unpackC (vars : (listof symbol)) (e1 : Expr) (e2 : Expr)]
            ;; Usage: (unpack (x1 x2 ... xn) e1 e2)
            [(unpack) (unpackC
                       (map s-exp->symbol (s-exp->list (second l)))
                       (parse (third l))
                       (parse (fourth l)))]
         )]
        )
      )
    ]
  )
)

;; Using the definitions taught in class.
;; Define a binding to add tuples.
;; Instead of (val : number) in the binding
;; we get the defn which returns an expression.
;; We will also save current env to achieve static scope.
;; Generally, env based execution is better than substitution based execution (due to just one pass of expression in env based).
(define-type Binding
  [bind (name : symbol) (env : Env) (defn : Expr)])

;; Define type aliases in racket for the environment we are going to use in binding.
(define-type-alias Env (listof Binding))

;; Define helper functions to define empty environment and function to extend an environment.
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : Env)) : Binding
  (cond
    [(cons? env)
     ;; True means environment non empty.
     (if (equal? x (bind-name (first env)))
         ;; We found the symbol in this environment! return.
         (first env)
         ;; We didn't find the symbol in the first of the list, so we recursively search through the rest of the list.
         (lookup x (rest env)))]
    ;; False means the environment is an empty list.
    [else (error 'lookup "No binding found")]))

(define (eval-env (env : Env) (e : Expr)) : Value
  (type-case Expr e
    ;; We return Value output directly if we encounter it.
    [valC (v) v]
    ;; Evaluates e1 and e2, which can be assumed to yield numbers n1 and n2, and returns their sum.
    [plusC (e1 e2) (numV (+ (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
    ;; Evaluates e1 and e2, which can be assumed to yield numbers n1 and n2, and returns their product.
    [timesC (e1 e2) (numV (* (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
    ;; We can just lookup this variable in env
    ;; But we have to get the lookup at the time of binding the variable
    [idC (x) (let [(b (lookup x env))]
               (eval-env (bind-env b) (bind-defn b)))]
    ;; Evaluates e1, which can be assumed to yield a non-negative integer n.
    ;; - If n is 0, evaluates e2 and returns the result.
    ;; - If n is > 0, first recursively executes (natrec (- n 1) e2 (x y e3)), to get some value v.
    ;;   Then evaluates e3 with x bound to (- n 1) and y bound to v; the result of evaluating e3 is returned.
    [natrecC (e1 e2 x y e3)
             (let ([n (numV-n (eval-env env e1))])
               (cond
                 [(= n 0) (eval-env env e2)]
                 [(> n 0)
                  (let* ([pred-val (numV (- n 1))]
                         [env-with-x (extend-env (bind x env (valC pred-val)) env)]
                         [rec-result (eval-env env-with-x 
                                              (natrecC (valC pred-val) e2 x y e3))]
                         [env-with-y (extend-env (bind y env-with-x (valC rec-result)) env-with-x)])
                    (eval-env env-with-y e3))]
                 ;; Handling error case
                 [(< n 0) (error 'natrecC "Evaluated first expression resulted less than 0")]))]
    ;; Evaluates e1 and e2, and returns true if e1 is equal to e2, and returns false otherwise.
    [equal?C (e1 e2)
             (boolV (equal? (eval-env env e1) (eval-env env e2)))]
    ;; Evaluates guard, which can be assumed to yield a boolean b.
    ;; - If b is true, evaluates e1 and returns the result.
    ;; - If b is false, evaluates e2 and returns the result.
    [ifC (g e1 e2)
         (if (boolV-b (eval-env env g))
             (eval-env env e1)
             (eval-env env e2))]
    ;; Evaluates e1 through en, yielding values v1 through vn, then returns the list containing v1 through vn.
    ;; es -> list of (e1, e2,...,en)
    [listC (es)
           (listV (map (lambda (ei) (eval-env env ei)) es))]
    ;; Evaluates e1 and e2, yielding v1 and v2 (which can be assumed to be a list),
    ;; and returns the list that adds v1 to the front of the list represented by v2.
    [consC (e1 e2)
           (listV (cons (eval-env env e1) (listV-vs (eval-env env e2))))]
    ;; Evaluates e yielding a value v (which can be assumed to be a non-empty list),
    ;; and returns the first element of v.
    [firstC (e)
            (first (listV-vs (eval-env env e)))]
    ;; Evaluates e yielding a value v (which can be assumed to be a non-empty list),
    ;; and returns the tail of v.
    [restC (e)
           (listV (rest (listV-vs (eval-env env e))))]
    ;; Evaluates e1 yielding a value v, which can be assumed to be a list.
    ;; - If the list v is empty, evaluates e2 and returns the result.
    ;; - If v is a non-empty list of the form (v1 v2 ... vn), first recursively
    ;;   evaluates (listrec (v2 ... vn) e2 (hd rest res e3)) to get some value vrec
    ;;   then evaluates e3 with hd bound to v1, rest bound to (v2 ... vn), and res bound to vrec.
    ;;   Returns the result of evaluating e3.
    [listrecC (e1 e2 hd rest1 res e3)
              (let ([lst (eval-env env e1)])
                (local [(define (listrec-helper (vs : (listof Value))) : Value
                          (cond
                            [(empty? vs) (eval-env env e2)]
                            [else
                             (let*
                                 ([env-with-hd (extend-env (bind hd env (valC (first vs))) env)]
                                    [env-with-rest1 (extend-env (bind rest1 env-with-hd (valC (listV (rest vs)))) env-with-hd)]
                                    [rec-result (listrec-helper (rest vs))]
                                    [env-with-res (extend-env (bind res env-with-rest1 (valC rec-result)) env-with-rest1)])
                               (eval-env env-with-res e3))]))]
                  (listrec-helper (listV-vs lst))))]
    ;; (let ([x1 e1] [x2 e2] ... [xn en]) e)
    ;; would be parsed as (letC (list (pair x1 e1) (pair x2 e2) ... (pair xn en)) e)
    ;; Behaves similarly to the racket "let" form: e1 through en are evaluated to get values v1 through vn,
    ;; then e is evaluated with xi bound to vi respectively.
    ;; We get a new environment with all the bindings and then evaluate recursively using this new environment.
    [letC (bindings e)
          (let ([new-env (foldl (lambda (b acc-env)
                                  (extend-env (bind (fst b) env (snd b)) acc-env))
                                env
                                bindings)])
            (eval-env new-env e))]
    ;; (let* ([x1 e1] [x2 e2] ... [xn en]) e)
    ;; would be parsed as (let*C (list (pair x1 e1) (pair x2 e2) ... (pair xn en)) e)
    ;; Behaves similarly to the racket "let*" form: e1 through en are evaluated to get values v1 through vn,
    ;; then e is evaluated with xi bound to vi respectively.
    ;; The difference between let is that in evaluating ei, the bindings for x1 through x_{i-1} are available.
    ;; When we are updating our new environment, we first evaluate with the current accumulated environment
    ;; and use this value to bind to current symbol and update the accumulated environment.
    ;; We then evaluate recursively using this new environment.
    [let*C (bindings e)
           (let ([new-env (foldl (lambda (b acc-env)
                                   (let ([val (eval-env acc-env (snd b))])
                                     (extend-env (bind (fst b) acc-env (valC val)) acc-env)))
                                 env
                                 bindings)])
             (eval-env new-env e))]
    ;; (unpack (x1 x2 ... xn) e1 e2)
    ;; would be parsed as (unpackC (list x1 x2 ... xn) e1 e2)
    ;; Evaluates e1, which can be assumed to yield a list l of the form (v1 v2 ... vn)
    ;; Then returns the result of evaluating e2 with x1 bound to v1, x2 bound to v2, ..., xn bound to vn.
    ;; It is assumed that the list l has the same length as the length of the 'vars' argument.
    [unpackC (vars e1 e2)
             (let ([lst (eval-env env e1)])
               (let ([vals (listV-vs lst)])
                 (cond
                   ;; Handling error case
                   [(not (equal? (length vars) (length vals))) (error 'unpackC "Length of variables doesn't match list length")]
                   [else
                    ;; We are binding all the variables in vars to all values (vls) we got from the lst
                    ;; Helper function is defined locally which is used to build the environment and recursively evaluate.
                    (local [(define (bind-vars (vs : (listof symbol))
                                               (vls : (listof Value))
                                               (acc-env : Env)) : Env
                              (cond
                                [(empty? vs) acc-env]
                                [else (bind-vars
                                       (rest vs)
                                       (rest vls)
                                       (extend-env (bind (first vs) env (valC (first vls))) acc-env))]))]
                      (eval-env (bind-vars vars vals env) e2))])))]))

;; We call eval-env with empty environment to evalauate the parsed s-expression.
(define (eval e)
  (eval-env empty-env e))