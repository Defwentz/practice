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

;; P38 (*) Compare the two methods of calculating Euler's totient function.
;;     Use the solutions of problems P34 and P37 to compare the algorithms. Take the number of logical inferences as a measure for efficiency. Try to calculate phi(10090) as an example.
;; First look, my totient-phi2 is much slower.

;; P39 (*) A list of prime numbers.
;;     Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
(define list-of-prime
  (lambda (bot top)
    (cond
     [(> bot top)
      (quote ())]
     [(is-prime bot)
      (cons bot (list-of-prime (add1 bot) top))]
     [else (list-of-prime (add1 bot) top)])))

;; P40 (**) Goldbach's conjecture.
;;     Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
;;     Example:
;;     * (goldbach 28)
;;     (5 23)
(define goldbach
  (lambda (num)
    (if (= num 2) '(1 1)
        (let G ((n (- num 2))
                (m 2))
          (cond
           [(and [is-prime n]
                 [is-prime m])
            (cons m (surround n))]
           [else (G (sub1 n) (add1 m))])))))

;; P41 (**) A list of Goldbach compositions.
;;     Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
;;     Example:
;;     * (goldbach-list 9 20)
;;     10 = 3 + 7
;;     12 = 5 + 7
;;     14 = 3 + 11
;;     16 = 3 + 13
;;     18 = 5 + 13
;;     20 = 3 + 17
;;     
;;     In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
;;     
;;     Example (for a print limit of 50):
;;     * (goldbach-list 1 2000 50)
;;     992 = 73 + 919
;;     1382 = 61 + 1321
;;     1856 = 67 + 1789
;;     1928 = 61 + 1867
(define goldbach-list
  (lambda (bot top limit)
    (define if-good-print
      (lambda (c)
        (let ((n (car c))
              (m (cadr c)))
          (if (and (> n limit)
                   (> m limit))
              (begin
                (display (string-append (number->string (+ m n))
                                        " = "
                                        (number->string n)
                                        " + "
                                        (number->string m)))
                (newline))))))
    (define get-even
      (lambda (deal n)
        (if (zero? (remainder n 2)) n
            (deal n))))
    (let ((bot (get-even add1 bot))
          (top (get-even sub1 top)))
      (let GL ((n bot))
        (if (= n top)
            (newline)
            (begin
              (if-good-print (goldbach n))
              (GL (+ 2 n))))))))
