#lang plai-typed
(require "ps3-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps3-ast.rkt and README.md for more information.

;; Note that as in lecture 6, you probably want to implement a version
;; of eval that returns a result that can be an arbitrary value (not just
;; a BaseValue) and also returns a store.  Your eval-base would then be a
;; wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be.
;;
;; For grading, the test cases all result in values that can be converted to base values.

#|
(define-type BaseValue
  [numBV (n : number)]
  [boolBV (b : boolean)]
  [pairBV (v1 : BaseValue) (v2 : BaseValue)])

(define-type Expr
  [numC (n : number)]
  [boolC (b : boolean)]
  [pairC (e1 : Expr) (e2 : Expr)]
  [fstC (e : Expr)]
  [sndC (e : Expr)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [equal?C (e1 : Expr) (e2 : Expr)]
  [ifC (guard : Expr) (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [boxC (e : Expr)]
  [unboxC (e : Expr)]
  [setboxC (e1 : Expr) (e2 : Expr)]
  [vectorC (es : (listof Expr))]
  [vector-lengthC (e : Expr)]
  [vector-refC (e1 : Expr) (e2 : Expr)]
  [vector-set!C (e1 : Expr) (e2 : Expr) (e3 : Expr)]
  [vector-makeC (e1 : Expr) (e2 : Expr)]
  [subvectorC (e : Expr) (offset : Expr) (len : Expr)]
  [beginC (es : (listof Expr))]
  [transactC (e : Expr)]
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
            [(pair) (pairC
                     (parse (second l))
                     (parse (third l)))]
            [(fst) (fstC (parse (second l)))]
            [(snd) (sndC (parse (second l)))]
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
            [(box) (boxC (parse (second l)))]
            [(set-box!) (setboxC (parse (second l)) (parse (third l)))]
            [(unbox) (unboxC (parse (second l)))]
            [(vector) (vectorC (map parse (rest l)))]
            [(vector-length) (vector-lengthC (parse (second l)))]
            [(vector-ref) (vector-refC
                           (parse (second l))
                           (parse (third l)))]
            [(vector-set!) (vector-set!C
                           (parse (second l))
                           (parse (third l))
                           (parse (fourth l)))]
            [(vector-make) (vector-makeC
                           (parse (second l))
                           (parse (third l)))]
            [(subvector) (subvectorC
                          (parse (second l))
                          (parse (third l))
                          (parse (fourth l)))]
            [(transact) (transactC (parse (second l)))]
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
  [pairV (v1 : Value) (v2 : Value)]
  ;; Closure which refers to lambda expression more or less
  [closV (env : Env) (x : symbol) (e : Expr)]
  ;; What box returns will be captured by this
  ;; We have to define somethings here to keep track of the boxed variables
  [boxV (l : Location)]
  [vectorV (locs : (vectorof Location))]
  )

;; Helper function to convert Value to BaseValue
(define (value->basevalue (v : Value)) : BaseValue
  (type-case Value v
    [numV (n) (numBV n)]
    [boolV (b) (boolBV b)]
    [pairV (v1 v2) (pairBV (value->basevalue v1) (value->basevalue v2))]
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
                      [pairV (v1a v1b)
                             (type-case Value v2
                               [pairV (v2a v2b)
                                      (and (equal-val? v1a v2a)
                                           (equal-val? v1b v2b))]
                               [else #f])]
                      [else #f]))

(define (eval-env (env : Env) (sto : Store) (e : Expr)) : Result
  (type-case Expr e
             [numC (n) (res (numV n) sto)]
             [boolC (b) (res (boolV b) sto)]
             ;; (pair e1 e2)
             ;; evaluates e1 and then e2, returns a pair containing the resulting values
             [pairC (e1 e2)
                    (type-case Result (eval-env env sto e1)
                      [res (v1 sto1)
                           (type-case Result (eval-env env sto1 e2)
                             [res (v2 sto2)
                                  (res (pairV v1 v2) sto2)])])]
             ;; (fst e)
             ;; evaluates e, which can be assumed to yield a pair, and then returns the first component of the pair.
             [fstC (e)
                   (type-case Result (eval-env env sto e)
                     [res (v sto1)
                          (res (pairV-v1 v) sto1)])]
             ;; (snd e)
             ;; evaluates e, which can be assumed to yield a pair, and then returns the second component of the pair.
             [sndC (e)
                   (type-case Result (eval-env env sto e)
                     [res (v sto1)
                          (res (pairV-v2 v) sto1)])]
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
             ; (lambda x e)
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
             ;; We evaluate e and box it and return
             ;; (box e)
             [boxC (e)
                   (type-case Result (eval-env env sto e)
                     [res (v sto1)
                          (let [(l (new-loc))]
                            (res (boxV l) (override-store (cell l v) sto1)))])] ;; This is where we add to the store
             ;; We get out the value of the box when unboxing
             ;; (unbox e)
             [unboxC (e)
                     (type-case Result (eval-env env sto e)
                       [res (v sto1)
                            (res (fetch (boxV-l v) sto1) sto1)])]
             ;; (set-box! e1 e2)
             [setboxC (e1 e2)
                      (type-case Result (eval-env env sto e1)
                        [res (v1 sto1)
                             (type-case Result (eval-env env sto e2)
                               [res (v2 sto2)
                                    (res v2 (override-store (cell (boxV-l v1) v2) sto2))])])]
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

             ;; Vector operations. As a general rule of thumb, these behave similarly to Racket's vector operations.

             ;; (vector e1 e2 ... en)
             ;; evaluates e1, then e2, then ..., en, yielding values v1 through vn
             ;; creates a mutable vector of length n containing these values and returns it.
             [vectorC (es)
                      (type-case ListResult (eval-list env sto es)
                        [listres (vs sto1)
                                 (let ([n (length vs)])
                                   ;; Create a new racket vector to hold all the vectors
                                   (let ([locvecs (make-vector n -1)]) ;; Create vector list with -1 initial values
                                     ;; A makeshift for loop to store the values in the above vector based on sto-final values
                                     (letrec
                                         ([loop (lambda (i vs-left sto-so-far) : Result
                                                  (if (= i n)
                                                      ;; Base case - we filled the vector so we just return it
                                                      (res (vectorV locvecs) sto-so-far)
                                                      ;; Recursive step
                                                      (let ([v (first vs-left)]
                                                            [l (new-loc)])
                                                        (begin
                                                          ;; Set the location l at index i in the vector we started with
                                                          (vector-set! locvecs i l)
                                                          ;; Recurse to increment i and update store
                                                          (loop (+ i 1) (rest vs-left) (override-store (cell l v) sto-so-far))))))])
                                       (loop 0 vs sto1))))])]
             ;; (vector-length e)
             ;; evaluates e, which can be assumed to yield a vector v, and then returns the length of v.
             [vector-lengthC (e)
                             (type-case Result (eval-env env sto e)
                               [res (v sto1) ;; v is a vectorV-locs
                                    (res (numV (vector-length (vectorV-locs v))) sto1)])]
             ;; (vector-ref e1 e2)
             ;; evaluates e1, which can be assumed to yield a vector v, then
             ;; evaluates e2, which can be assumed to yield an integer number n
             ;; such that 0 <= n <= length of v, and then returns the value at
             ;; position n in the vector v
             [vector-refC (e1 e2)
                          (type-case Result (eval-env env sto e1)
                            [res (v sto1) ;; v is a vectorV-locs
                                 (type-case Result (eval-env env sto1 e2)
                                   [res (n sto2) ;; n is a numV-n
                                        (let ([l (vector-ref (vectorV-locs v) (numV-n n))]) ;; Get the Location of the vector at n number
                                          (res (fetch l sto2) sto2))])])]
             ;; (vector-set! e1 e2 e3)
             ;; evaluates e1, which can be assumed to yield a vector v, then
             ;; evaluates e2, which can be assumed to yield an integer number n
             ;; such that 0 <= n <= length of v, and then evaluates e3, yielding some value v'
             ;; then it updates position n at vector v to be the value v'
             [vector-set!C (e1 e2 e3)
                           (type-case Result (eval-env env sto e1)
                            [res (v sto1) ;; v is a vectorV-locs
                                 (type-case Result (eval-env env sto1 e2)
                                   [res (n sto2) ;; n is a numV-n
                                        (type-case Result (eval-env env sto2 e3)
                                          [res (val sto3) ;; val is value to set at nth position in v
                                               (let ([l (vector-ref (vectorV-locs v) (numV-n n))]) ;; Get the Location of the vector at n number
                                                 (res val (override-store (cell l val) sto3)))])])])]
             ;; (vector-make e1 e2)
             ;; evaluates e1 which should yield an integer number n such that 0 <= n,
             ;; then evalutes e2 to a value v.
             ;; allocated a mutable vector of length n where each entry in the vector is of value v
             ;; (Note: in actual Racket this is called make-vector instead)
             [vector-makeC (e1 e2)
                           (type-case Result (eval-env env sto e1)
                             [res (vn sto1) ;; vn is a numV-n (length of vector we require)
                                  (type-case Result (eval-env env sto1 e2)
                                    [res (v sto2) ;; v is a Value (value to be stored in the vector we create)
                                         ;; Create a new racket vector to hold all the vectors
                                         (let* ([n (numV-n vn)]
                                                [locvecs (make-vector n -1)])
                                           (letrec
                                               ([loop (lambda (i sto-so-far) : Result
                                                  (if (= i n)
                                                      ;; Base case - we filled the vector so we just return it
                                                      (res (vectorV locvecs) sto-so-far)
                                                      ;; Recursive step
                                                      (let ([l (new-loc)])
                                                        (begin
                                                          ;; Set the location l at index i in the vector we started with
                                                          (vector-set! locvecs i l)
                                                          ;; Recurse to increment i and update store
                                                          (loop (+ i 1) (override-store (cell l v) sto-so-far))))))])
                                             (loop 0 sto2)))])])]
             ;; (subvector e1 offset len)
             ;; evaluates e1, which should yield a vector v
             ;; then evaluates offset, which should yield an integer number n such that 0 <= n <= length v
             ;; then evaluates len, which should yield an integer number l such that 0 <= l and (n + l) <= length v
             ;; returns a vector value v' of length l that refers to the same vector location as the original vector v,
             ;; but where the v' vector's position i refers to the same location as
             ;; positon (i + offset) in the original vector v
             ;;
             ;; e.g. if v is a vector and we were to do
             ;; (let v (vector 1 2 3 4)
             ;;   (begin (vector-set! (subvector v 1 3) 0 10)
             ;;          (vector-ref v 1)))
             ;; the result would yield 10, because the modification of the subvector
             ;; affects the original vector v at position 1
             [subvectorC (e1 offset len)
                         (type-case Result (eval-env env sto e1)
                           [res (v sto1) ;; v is a vectorV
                                (type-case Result (eval-env env sto1 offset)
                                  [res (vn sto2) ;; vn is an integer (numV-n) (integer number n)
                                       (type-case Result (eval-env env sto2 len)
                                         [res (vl sto3) ;; vl is an integer (numV-n) (integer number l)
                                              (let* ([n (numV-n vn)]
                                                     [l (numV-n vl)]
                                                     [oldlocsvec (vectorV-locs v)]
                                                     ;; Create a new racket vector to hold the new vector list to be created
                                                     [newlocsvec (make-vector l -1)])
                                                (letrec
                                                    ([loop (lambda (i) : Result
                                                             (if (= i l)
                                                                 ;; Base case: we filled the vector so we just return it
                                                                 (res (vectorV newlocsvec) sto3)
                                                                 ;; Recursive step
                                                                 (begin
                                                                     ;; Set the location at index i of new vector as the one in old vector from the offset given
                                                                     (vector-set! newlocsvec i (vector-ref oldlocsvec (+ i n)))
                                                                     ;; Recurse to increment i and update store
                                                                     (loop (+ i 1)))))])
                                                  ;; Start the loop
                                                  (loop 0)))])])])]
             ;; (transact e)
             ;; evaluates e, which should yield a pair p, where the first component of the pair p is a boolean b
             ;; if b is true, it returns the second component of p
             ;; if b is false, it returns the second component of p,
             ;; but all of the memory modifications performed during executing e are undone when (transact e) finishes.
             [transactC (e)
                        (type-case Result (eval-env env sto e)
                          [res (p sto1) ;; p is a pairV
                               (if (boolV-b (pairV-v1 p))
                                   (res (pairV-v2 p) sto1)
                                   (res (pairV-v2 p) sto))])]
                                   
                      
             #;[else (error 'eval-env "Unimplemeneted")]
             )
  )

(define (eval (e : Expr))
  (eval-env empty-env empty-store e))

(define (eval-base (e : Expr)) : BaseValue
  (type-case Result (eval e)
    [res (v sto)
         (value->basevalue v)]))
