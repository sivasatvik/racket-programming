#lang racket
(provide (struct-out bt-node) (struct-out bt-leaf) (struct-out bt-empty))
(provide (struct-out plus-node) (struct-out times-node) (struct-out int-leaf))
(provide palindrome-list eval-tree check-bt ml-split piles-insert)

; For each problem, you can find additional test cases and examples by looking in ps1-test.rkt.
; The general format of each test is of the form
;
;  (test-equal? "name" (f arg) expected)
;
; where f is a function you need to implement, arg is some test case arguments, and expected is what
; should be returned in that case. 
;
; If you don't understand what a problem is asking, look at the test cases for clarification.
; You can test your code by putting your ps1.rkt in the same directory as ps1-test.rkt and then
; running ps1-test.rkt in DrRacket  or running "racket ps1-sol-test.rkt"
; from the command line.

; Submit your code by uploading the completed ps1.rkt to GradeScope.

;;;;;;;;; Problem 1 ;;;;;;;;;

; Recall that a palindrome or (palindromic word) is a word that is the
; same when read forwards and backwards.  For example, "kayak", "dad",
; and "radar", are all palindromes.

; Write a function palindrome-list which takes a string s as an argument
; and returns a list of all of the palindromic words that occur in s
; when s is converted to lower-case letters and all punctuation is
; removed.  The words should occur in the list in the order that they
; occur in the original string.  If a palindrome occurs multiple times,
; each occurrence should be in the list.

; Example: when s is "The gig was a gag, a joke.", (palindrome-list s) should return
; '("gig" "a" "gag" "a").

;; ############################ SOLUTION ############################
;; We define a function to check if a given string is palindrome
;; We will find if the string is palindrome by checking if it's equal to the reverse of itself
(define (is-palindrome? w)
  (and
   (> (string-length w) 0)
   (equal? w (list->string (reverse (string->list w))))))

;; We also define a function which will remove all the unnecessary characters other than
;; alphabets and numbers in a given word
(define (cleaned w)
  (regexp-replace* #px"[^a-zA-Z0-9 ]" (string-downcase w) ""))

;; We also define a function to give us the list of cleaned words from the given string
(define (cleaned-words lst) (string-split (cleaned lst)))

;; We finally call the function to find if each word in the cleaned-worlds list is palindrome or not
;; and apply filter to get only the true words
(define (palindrome-list s)
  (filter is-palindrome? (cleaned-words s))
)


;;;;;;;;; Problem 2 ;;;;;;;;;

; In this problem we consider trees that are constructed using
; plus-node, times-node, or int-leaf.  The arg1 and arg2 fields of
; plus-node and times-node should be themselves trees constructed from
; these constructors. The val field of int-leaf should be an integer.

; We shall think of these trees as representing arithmetic expressions,
; Where, for example, (int-leaf i) represents the number i, plus-node e1
; e2, represents e1 + e2, after interpreting the trees e1 and e2 as
; arithmetic expressions, and similarly for times-node e1 e2.
; 
; Write a function eval-tree, which takes a tree t as input and returns
; the integer that results from evaluating the arithmetic expression
; corresponding to the tree.  For example (eval-tree (plus-node
; (int-leaf 1) (int-leaf 2))) should return 3.


(struct plus-node (arg1 arg2))
(struct times-node (arg1 arg2))
(struct int-leaf (val))

;; ############################ SOLUTION ############################
(define (eval-tree t)
  (match t
    ;; If the curent node is an int-leaf, we just return the value -> Base case
    [(int-leaf v) v]
    ;; If the current node is a plus-node, we will do the actual addition with arg1 and arg2 recursively and return
    [(plus-node a1 a2) (+ (eval-tree a1) (eval-tree a2))]
    ;; If the current node is a plus-node, we will do the actual multiplication with arg1 and arg2 recursively and return
    [(times-node a1 a2) (* (eval-tree a1) (eval-tree a2))]
    ;; If we match with something other than the above three, return an error
    [else (error `eval "Invalid node")]
    ))

;;;;;;;;; Problem 3 ;;;;;;;;;;;;

; In this problem we consider binary trees that are constructed using
; bt-node, bt-leaf, and bt-empty. int-leaf.

; The val fields of bt-node and bt-leaf are integers. The left and right
; fields of bt-node should be themselves trees constructed from these
; constructors.

; Recall that a binary search tree is a binary tree in which we have an
; invariant requiring that for a node of the form (bt-node i lt rt),
; every node value in the left child tree lt should be smaller than i,
; and every node in the right tree should be larger than i.

; Write a function check-bt which takes as an argument a binary tree t
; constructed using the above structs, and returns #t if t satisfies
; the binary tree invariant, and #f otherwise.

; Example: (check-bt (bt-node 5 (bt-leaf 1) (bt-leaf 6))) should return #t,
; but (check-bt (bt-node 5 (bt-leaf 6) (bt-leaf 6))) should return #f.


(struct bt-node (val left right))
(struct bt-leaf (val))
(struct bt-empty ())

;; ############################ SOLUTION ############################
;; We will create bounds for the values in the node/leaf. As we go down the tree, we will
;; check if the values in them fall in that range to validate whether the tree falls under BST.
;; Initial min and max value will be -inf and +inf respectively.
(define (check-bt-helper tree min-val max-val)
  (cond
    ;; Return true if the current node is empty node -> Base case
    [(bt-empty? tree) #t]
    ;; If the current node is a leaf we need to return true
    ;; only if the value in the leaf falls under min and max value
    [(bt-leaf? tree)
     (let ([current-val (bt-leaf-val tree)])
       (and
        (< current-val max-val)
        (> current-val min-val)))]
    ;; If the current node is a node, we need to check
    ;; if the value in the node falls under min and max range
    ;; and recurse on the left and right subtree by updating the
    ;; max value and min value with current node value respectively.
    [(bt-node? tree)
     (let ([current-val (bt-node-val tree)]
           [left-tree (bt-node-left tree)]
           [right-tree (bt-node-right tree)])
       (and
        (< current-val max-val)
        (> current-val min-val)
        (check-bt-helper left-tree min-val current-val)
        (check-bt-helper right-tree current-val max-val)))]))

;; We call the helper function with initial min and max values
(define (check-bt tree)
  (check-bt-helper tree -inf.0 +inf.0))

;;;;;;;;; Problem 4 ;;;;;;;;;;;;

; Given a list of integers, '(i1 i2 ... ik), we say that the list is
; strictly monotone if either i1 < i2 < ... < ik or i1 > i2 > ... > ik.
; i.e. for either every element in the list is strictly smaller than the
; next element, or every element is strictly greater than the next
; element.
; 
; Write a function ml-split which takes a list l of integers and returns
; a list of lists obtained by breaking up l into strictly monotone
; lists. The returned lists should be maximal, meaning that there is no
; other splitting of l into strictly monotone lists in which any of the
; lists in the list could be larger while still being montone. In the
; case of ties, your solution should prefer to make the earlier lists
; larger.
; 
; Example: (ml-split '(1 2 3 4 3 2 1)) should return '((1 2 3 4) (3 2
; 1)).  Returning '((1 2) (3 4) (3 2 1)) would be wrong, because while
; each of the lists is strictly monotone, we could combine the first two
; to '(1 2 3 4), which is larger. Similarly, returning '((1 2 3) (4 3 2
; 1)) would be wrong because we should prefer to make the first list
; larger at the expense of making the second list shorter.


;; ############################ SOLUTION ############################
;; We define a direction function which tells us whether the list is increasing
;; or decreasing at a given location in the list based on the difference with
;; previous integer.
(define (direction n)
  (cond
    [(< n 0) 'dec]
    [(> n 0) 'inc]
    [else 'eq]
    ))

;; We define a helper loop function. It takes arguments the following
;; next -> the rest of the elements on the right of the current element
;; curr -> the current list of elements we are working to split
;; dir -> the direction of current list of elements ('inc/'dec'/'eq)
;; acc -> the accumulator of the lists on the left till now
;; The function does the following
;; If the next elements are not there in the list,
;; we will cons the current list of elements with the accumulator. -> Base case
;; In the inductive step, we will compare the elements in the next list of elements
;; with the current element and based on its relative direction.
(define (loop next curr dir acc)
  (cond
    ;; Base case -> We return the accumulator appropriately
    [(empty? next) (reverse (cons (reverse curr) acc))]
    [else (let* ([x (car next)]
                 [last (car curr)]
                 [relative (direction (- x last))])
            (cond
              ;; If the previous direction is 'eq
              [(equal? 'eq dir)
               (if (equal? 'eq relative)
                   ;; And current relative direction is also 'eq
                   ;; We will start to construct new list recursively and update our accumulator and preserve the direction
                   (loop (rest next) (list x) 'eq (cons (reverse curr) acc))
                   ;; And if the current relative direction is something else,
                   ;; we will propagate this direction and keep constructing the current list recursively
                   (loop (rest next) (cons x curr) relative acc))]
              ;; If the previous direction and current relative direction are equal,
              ;; then we keep constructing the current list recursively preserving the direction
              [(equal? relative dir) (loop (rest next) (cons x curr) dir acc)]
              ;; In else case, we will start to construct new list recursively with direction 'eq
              ;; and update our accumulator with the current list
              [else (loop (rest next) (list x) 'eq (cons (reverse curr) acc))])
            )]))

;; We call the loop helper function with initial diretion as 'wq and empty accumulator
;; and we also handle the base case as an emptly list input
(define (ml-split l)
  (cond
    ;; If list is empty, we return empty list -> Base case
    [(empty? l) `()]
    ;; Else, we loop through the list and split it accordingly by calling the helper loop function
    [else (loop (rest l) (list (car l)) 'eq `())]))

;;;;;;;;; Problem 5 ;;;;;;;;;;;;

; Write a function piles-insert which takes two arguments.  When running
; (piles-insert ls n), the first argument ls is a list of lists of
; integers. We call each list in ls a pile. You may assume that each
; pile is non-empty, and that each pile is sorted in ascending
; order. Finally, you may also assume that if ls is of the form '(p_1
; p_2 ... p_n), then the head of p_i is strictly less than the head of p_(i+1).
; 
; Evaluating (piles-insert ls n) should return a list of piles obtained from
; taking ls and inserting n so that either n has
; (1) been added to the head of the first pile in ls
; whose previous head is greater than or equal to n, or
; (2) if no such pile exists, then a new pile containing
; just n is added to the end of ls.

; Example: (piles-insert '((4) (5)) 3) should return '((3 4) (5))) and
; (piles-insert '((2) (6)) 4) should return '((2) (4 6)))

;; ############################ SOLUTION ############################
;; We define a helper function to go through the piles and insert n at the right location.
;; In this helper function, we want to have an accumulator
;; which keeps track of the piles we visited so far in reverse order
;; because we can cons the element to the list we created so far at the front.
;; When we find the right location, we will cons the element to the current pile at the front
;; and cons this to the tail of the list. And we also have to add the accumulator (in reverse order)
;; till that point to the front of this list we have.
(define (piles-insert-helper acc ls n)
  (cond
    ;; If the list is null, we will cons n to the accumulator and return the reverse of the list -> Base case
    [(null? ls)
     (reverse (cons (list n) acc))]
    [else
     ;; In else case, we keep track of current pile and its head
     ;; and depending of head value relative to n, we will either append n to pile and return
     ;; or recursively call the helper function after appending the current pile to accumulator.
     (let* ([pile (car ls)]
           [head (car pile)])
       (if (<= n head)
           (append (reverse acc) (cons (cons n pile) (cdr ls)))
           (piles-insert-helper (cons pile acc) (cdr ls) n)))]))

;; We call the helper function with an empty accumulator
(define (piles-insert ls n)
  (piles-insert-helper '() ls n))
