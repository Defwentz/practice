(load "lists.scm")
;; P31 (**) Determine whether a given integer number is prime.
;;     Example:
;;     * (is-prime 7)
;;     T
(define is-prime
  (lambda (num)
    (letrec ((I (lambda (count)
                  (if (= count 1)
                      #t
                      (if (zero? (remainder num count))
                          #f
                          (I (sub1 count)))))))
      (I (sub1 num)))))

;; P32 (**) Determine the greatest common divisor of two positive integer numbers.
;;     Use Euclid's algorithm.
;;     Example:
;;     * (gcd 36 63)
;;     9
(define gcd
  (lambda (a b)
    (if (zero? b)
        a
        (gcd b (remainder a b)))))

;; P33 (*) Determine whether two positive integer numbers are coprime.
;;     Two numbers are coprime if their greatest common divisor equals 1.
;;     Example:
;;     * (coprime 35 64)
;;     T
(define coprime
  (lambda (a b)
    (if (= (gcd a b) 1)
        #t #f)))

;; P34 (**) Calculate Euler's totient function phi(m).
;;     Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
;;     Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
;;     
;;     * (totient-phi 10)
;;     4
;;     
;;     Find out what the value of phi(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).
(define totient-phi
  (lambda (m)
    (let TP ((r 1))
      (cond
       [(= r m) 0]
       [(coprime r m)
        (add1 (TP (add1 r)))]
       [else (TP (add1 r))]))))

;; P35 (**) Determine the prime factors of a given positive integer.
;;     Construct a flat list containing the prime factors in ascending order.
;;     Example:
;;     * (prime-factors 315)
;;     (3 3 5 7)
(define prime-factors
  (lambda (num)
    (let PF ((mnum num)
             (count 2))
      (cond
       [(not (> num count))
        (quote ())]
       [(zero? (remainder mnum count))
        (cons count
              (PF (/ mnum count) count))]
       [else (PF mnum (next-prime count))]))))
(define next-prime
  (lambda (num)
    (let NP ((mnum (add1 num)))
      (if (is-prime mnum)
          mnum
          (NP (add1 mnum))))))

;; P36 (**) Determine the prime factors of a given positive integer (2).
;;     Construct a list containing the prime factors and their multiplicity.
;;     Example:
;;     * (prime-factors-mult 315)
;;     ((3 2) (5 1) (7 1))
;;     Hint: The problem is similar to problem P13.
(define prime-factors-mult
  (lambda (num)
    ((new-encode (encode-cons
                  (lambda (elem)
                    (cons elem
                          (surround 1)))
                  (lambda (count elem)
                    (cons elem
                          (surround count)))))
     (prime-factors num))))

;; P37 (**) Calculate Euler's totient function phi(m) (improved).
;;     See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
;;     phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
;;              (p2 - 1) * p2 ** (m2 - 1) *
;;              (p3 - 1) * p3 ** (m3 - 1) * ...
;; ERROR there is an error in here http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
;; it should be multiplication instead of addition.
;;     
;;     Note that a ** b stands for the b'th power of a.
(define totient-phi2
  (lambda (m)
    (let P ((pfct-mult (prime-factors-mult m)))
      (if (null? pfct-mult)
          1
          (let ([pn (caar pfct-mult)]
                [mn (cadar pfct-mult)])
            (* (* (sub1 pn)
                  (expt pn
                        (sub1 mn)))
               (P (cdr pfct-mult))))))))
