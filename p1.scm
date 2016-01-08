;;
;; 99 lisp problems
;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
;;

(define (sub1 x) (- x 1))
(define (add1 x) (+ x 1))
(define (atom? x) (and [not (pair? x)]
                       [not (null? x)]))

;; P01 (*) Find the last box of a list.
;;     Example:
;;     * (my-last '(a b c d))
;;     (D)
(define my-last
  (lambda (l)
    (cond
     [(null? l)
      (quote ())]
     [(null? (cdr l))
      l]
     [else
      (my-last (cdr l))])))

;; P02 (*) Find the last but one box of a list.
;;     Example:
;;     * (my-but-last '(a b c d))
;;     (C D)
(define my-but-last
  (lambda (l)
    (cond
     [(null? l)
      (quote ())]
     [(or (null? (cdr l))
          (null? (cdr (cdr l))))
      l]
     [else (my-but-last (cdr l))])))

;; P03 (*) Find the K'th element of a list.
;;     The first element in the list is number 1.
;;     Example:
;;     * (element-at '(a b c d e) 3)
;;     C
(define element-at
  (lambda (l at)
    (cond
     [(zero? at)
      (quote ())]
     [(zero? (sub1 at))
      (car l)]
     [else (element-at (cdr l) (sub1 at))])))

;; P04 (*) Find the number of elements of a list.
(define length
  (lambda (l)
    (cond
     [(null? l) 0]
     [else
      (add1 (length (cdr l)))])))

;; P05 (*) Reverse a list.
;; my original thoughts:
(define reverse
  (lambda (l)
    (cond
     [(null? l)
      (quote ())]
     [else (cons (element-at l (length l))
                 (reverse (rid-of-last l)))])))
(define rid-of-last
  (lambda (l)
    (cond
     [(null? (cdr l))
      (cdr l)]
     [else (cons (car l)
                 (rid-of-last (cdr l)))])))
;; after looking at another solution...:
(define reverse-2
  (lambda (l)
    (letrec ((R (lambda (l res)
                  (cond
                   [(null? l)
                    res]
                   [else (R (cdr l)
                            (cons (car l) res))]))))
      (R l (quote ())))))

;; P06 (*) Find out whether a list is a palindrome.
;;     A palindrome can be read forward or backward; e.g. (x a m a x).
;; my original thoughts:
(define palindrome?
  (lambda (l)
    (cond
     [(or (null? l)
          (null? (cdr l)))
      #t]
     [(eq? (car l)
           (element-at l (length l)))
      (palindrome? (rid-of-last (cdr l)))]
     [else #f])))
;; after looking at another solution...:
(define palindrome?-2
  (lambda (l)
    (equal? l (reverse-2 l))))

;; P07 (**) Flatten a nested list structure.
;;     Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
;;
;;     Example:
;;     * (my-flatten '(a (b (c d) e)))
;;     (A B C D E)
;;
;;     Hint: Use the predefined functions list and append.
(define my-flatten
  (lambda (l)
    (cond
     [(null? l)
      (quote ())]
     [(atom? (car l))
      (cons [car l]
            [my-flatten (cdr l)])]
     [else
      (append [my-flatten (car l)]
              [my-flatten (cdr l)])])))

;; P08 (**) Eliminate consecutive duplicates of list elements.
;;     If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
;;
;;     Example:
;;     * (compress '(a a a a b c c a a d e e e e))
;;     (A B C A D E)
(define compress
  (lambda (lat)
    (letrec ((C (lambda (l elem)
                  (cond
                   [(null? l)
                    (quote ())]
                   [(eq? (car l) elem)
                    (C (cdr l) elem)]
                   [else (cons (car l)
                               (C (cdr l) (car l)))]))))
      (cond
       [(null? lat) lat]
       [else (cons (car lat)
                   (C (cdr lat) (car lat)))]))))

;; P09 (**) Pack consecutive duplicates of list elements into sublists.
;;     If a list contains repeated elements they should be placed in separate sublists.
;;     
;;     Example:
;;     * (pack '(a a a a b c c a a d e e e e))
;;     ((A A A A) (B) (C C) (A A) (D) (E E E E))
(define pack
  (lambda (lat)
    (letrec ((P (lambda (l elem tmp-res)
                  (cond
                   [(null? l)
                    (cons tmp-res
                          (quote ()))]
                   [(eq? (car l) elem)
                    (P (cdr l) elem (cons (car l)
                                     tmp-res))]
                   [else (cons tmp-res
                               (P (cdr l) (car l) (cons (car l)
                                                   (quote ()))))]))))
      (cond
       [(null? lat) lat]
       [else (P (cdr lat) (car lat) (cons (car lat)
                                        (quote ())))]))))

;; P10 (*) Run-length encoding of a list.
;;     Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
;;     
;;     Example:
;;     * (encode '(a a a a b c c a a d e e e e))
;;     ;;((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
(define cons-encode
    (lambda (elem count)
      (cons count
            (cons elem
                  (quote ())))))
(define encode
  (lambda (lat)
    (letrec ((E (lambda (l elem count)
                  (cond
                   [(null? l)
                    (cons (cons-encode elem count)
                          (quote ()))]
                   [(eq? (car l) elem)
                    (E (cdr l) elem (add1 count))]
                   [else (cons (cons-encode elem count)
                               (E (cdr l) (car l) 1))]))))
      (cond
       [(null? lat) (quote ())]
       [else (E (cdr lat) (car lat) 1)]))))
(encode '(a a a a b c c a a d e e e e))

;; P11 (*) Modified run-length encoding.
;;     Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
;;     
;;     Example:
;;     * (encode-modified '(a a a a b c c a a d e e e e))
;;     ((4 A) B (2 C) (2 A) D (4 E))
(define cons-encode-modified
    (lambda (elem count)
      (cond
       [(zero? (sub1 count))
        elem]
       [else
        (cons count
              (cons elem
                    (quote ())))])))
(define encode-modified
  (lambda (lat)
    (letrec ((E (lambda (l elem count)
                  (cond
                   [(null? l)
                    (cons (cons-encode-modified elem count)
                          (quote ()))]
                   [(eq? (car l) elem)
                    (E (cdr l) elem (add1 count))]
                   [else (cons (cons-encode-modified elem count)
                               (E (cdr l) (car l) 1))]))))
      (cond
       [(null? lat) (quote ())]
       [else (E (cdr lat) (car lat) 1)]))))
(encode-modified '(a a a a b c c a a d e e e e))

;; P12 (**) Decode a run-length encoded list.
;;     Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
(define decode-helper
  (lambda (count elem)
    (cond
     [(zero? count)
      (quote ())]
     [else (cons elem
                 (decode-helper (sub1 count) elem))])))
(define decode
  (lambda (lc)
    (cond
     [(null? lc) lc]
     [(atom? (car lc))
      (cons (car lc)
            (decode (cdr lc)))]
     [else (cons (decode-helper (car (car lc))
                                (car (cdr (car lc))))
                   (decode (cdr lc)))])))
(decode (encode-modified '(a a a a b c c a a d e e e e)))

;; P13 (**) Run-length encoding of a list (direct solution).
;;     Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
;;     
;;     Example:
;;     * (encode-direct '(a a a a b c c a a d e e e e))
;;     ((4 A) B (2 C) (2 A) D (4 E))
;; same as P11

;; P14 (*) Duplicate the elements of a list.
;;     Example:
;;     * (dupli '(a b c c d))
;;     (A A B B C C C C D D)
(define dupli
  (lambda (lat)
    (cond
     [(null? lat) lat]
     [else (cons (car lat)
                 (cons (car lat)
                       (dupli (cdr lat))))])))

;; P15 (**) Replicate the elements of a list a given number of times.
;;     Example:
;;     * (repli '(a b c) 3)
;;     (A A A B B B C C C)
(define repli-helper
  (lambda (elem t rst)
    (letrec ((N-E (lambda (count)
                    (cond
                     [(zero? count)
                      (repli rst t)]
                     [else (cons elem
                                 (N-E (sub1 count)))]))))
      (N-E t))))
(define repli
  (lambda (lat t)
    (cond
     [(null? lat) (quote ())]
     [else (repli-helper (car lat) t (cdr lat))])))
(repli '(a b c) 3)

;; P16 (**) Drop every N'th element from a list.
;;     Example:
;;     * (drop '(a b c d e f g h i k) 3)
;;     (A B D E G H K)
(define drop
  (lambda (lat n)
    (letrec ((D (lambda (l count)
                  (cond
                   [(null? l) (quote ())]
                   [(zero? (sub1 count))
                    (drop (cdr l) n)]
                   [else (cons (car l)
                               (D (cdr l) (sub1 count)))]))))
      (D lat n))))

;; P17 (*) Split a list into two parts; the length of the first part is given.
;;     Do not use any predefined predicates.
;;     
;;     Example:
;;     * (split '(a b c d e f g h i k) 3)
;;     ((A B C) (D E F G H I K))
(define split
  (lambda (lat length-of-first)
    (let ([first-half (quote ())]
          [other-half (quote ())])
      (letrec ((S (lambda (l count)
                    (cond
                     [(zero? (sub1 count))
                      (set! other-half (cdr l))
                      (cons (car l)
                            (quote ()))]
                     [else (cons (car l)
                                 (S (cdr l) (sub1 count)))]))))
        (set! first-half (S lat length-of-first))
        (cons first-half
              (cons other-half
                    (quote ())))))))
