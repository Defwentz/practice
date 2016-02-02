(load "lists.scm")
;; P46 (**) Truth tables for logical expressions.
;;     Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed. Note that A and B can be Prolog goals (not only the constants true and fail).
;;     A logical expression in two variables can then be written in prefix notation, as in the following example: and(or(A,B),nand(A,B)).
;;     
;;     Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
;;     
;;     Example:
;;     * table(A,B,and(A,or(A,B))).
;;     true true true
;;     true fail true
;;     fail true fail
;;     fail fail fail
;; (table 'A 'B '(and A (or A B)))

;; P47 (*) Truth tables for logical expressions (2).
;;     Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.
;;     Example:
;;     * table(A,B, A and (A or not B)).
;;     true true true
;;     true fail true
;;     fail true fail
;;     fail fail fail
;; (table 'A 'B '(A and (A or (not B))))

;; P48 (**) Truth tables for logical expressions (3).
;;     Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.
;;     Example:
;;     * table([A,B,C], A and (B or C) equ A and B or A and C).
;;     true true true true
;;     true true fail true
;;     true fail true true
;;     true fail fail true
;;     fail true true true
;;     fail true fail true
;;     fail fail true true
;;     fail fail fail true
;; (table '(A B C) '((A and (B or C)) equ (A and (B or (A and C)))))

(define table
  (lambda (vars sexp)
    (let ((cnt empty-cnt))
      (letrec ((go-through-vars (lambda (vs)
                                  (cond
                                   [(null? vs)
                                    (print-vars)
                                    (display (my-eval sexp cnt))
                                    (newline)]
                                   [else
                                    (let ((avs (car vs)))
                                      (set! cnt (put-cnt cnt (car vs) #t))
                                      (go-through-vars (cdr vs))
                                      (set! cnt (put-cnt cnt (car vs) #f))
                                      (go-through-vars (cdr vs)))])))
               (print-vars (lambda ()
                             (let P ((vs vars))
                               (cond
                                [(null? vs)
                                 (display "")]
                                [else (display (look cnt (car vs)))
                                      (display " ")
                                      (P (cdr vs))])))))
        (go-through-vars vars)))))

(define operator
  (lambda (o)
    (cond
     [(eq? 'and o) and/2]
     [(eq? 'or o) or/2]
     [(eq? 'nand o) nand/2]
     [(eq? 'nor o) nor/2]
     [(eq? 'xor o) xor/2]
     [(eq? 'impl o) impl/2]
     [(eq? 'equ o) equ/2])))
(define (and/2 a b) (and a b))
(define (or/2 a b) (or a b))
(define (nand/2 a b) (not (and a b)))
(define (nor/2 a b) (not (or a b)))
(define (xor/2 a b) (cond
                     [a (not b)]
                     [b #t]
                     [else #f]))
(define (impl/2 a b) (or (not a) b))
(define (equ/2 a b) (cond
                     [a b]
                     [else (not b)]))
(define put-cnt
  (lambda (cnt k v)
    (lambda (i)
      (if (eq? i k) v
          (cnt i)))))
(define empty-cnt 0)
(define (look cnt i) (cnt i))
(define get-op cadr)
(define get-first-sexp car)
(define get-second-sexp caddr)

(define my-eval
  (lambda (sexp cnt)
    (cond
     [(atom? sexp)
      (if (boolean? sexp)
          sexp
          (look cnt sexp))]
     [(null? (cdr sexp))
      (if (boolean? (car sexp))
          (car sexp)
          (look cnt (car sexp)))]
     [(eq? 'not (car sexp))
      (not (my-eval (cdr sexp) cnt))]
     [else ((operator (get-op sexp))
            (my-eval (get-first-sexp sexp) cnt)
            (my-eval (get-second-sexp sexp) cnt))])))

;; P49 (**) Gray code.
;;     An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
;;     n = 1: C(1) = ['0','1'].
;;     n = 2: C(2) = ['00','01','11','10'].
;;     n = 3: C(3) = ['000','001','011','010',�110�,�111�,�101�,�100�].
;;     
;;     Find out the construction rules and write a predicate with the following specification:
;;     
;;     % gray(N,C) :- C is the N-bit Gray code
;;     
;;     Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?
(define gray
  (lambda (N)
    (letrec ([n-zero (lambda (N)
                          (if (zero? N) ""
                              (string-append "0"
                                             (n-zero (sub1 N)))))]
             [append-x (lambda (x)
                         (lambda (str)
                           (string-append x str)))])
      (if (= N 1)
          '("0" "1")
          (append (map (append-x "0") (gray (sub1 N)))
                  (map (append-x "1") (reverse (gray (sub1 N)))))))))
