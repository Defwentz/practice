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
