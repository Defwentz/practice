(load "lists.scm")
(define-syntax letcc
  (syntax-rules ()
    ((letcc var body ...)
     (call-with-current-continuation
      (lambda (var)  body ... )))))
;; A binary tree is either empty or it is composed of a root element and two successors, which are binary trees themselves.  
;; In Lisp we represent the empty tree by 'nil' and the non-empty tree by the list (X L R), where X denotes the root node and L and R denote the left and right subtree, respectively. The example tree depicted opposite is therefore represented by the following list: 

;; (a (b (d nil nil) (e nil nil)) (c nil (f (g nil nil) nil))) 

;; Other examples are a binary tree that consists of a root node only:

;; (a nil nil) or an empty binary tree: nil.

;; You can check your predicates using these example trees. They are given as test cases in p54.lisp.

;; P54A (*) Check whether a given term represents a binary tree
;;      Write a predicate istree which returns true if and only if its argument is a list representing a binary tree.
;;       Example:
;;       * (istree '(a (b nil nil) nil))
;;       T
;;       * (istree '(a (b nil nil)))
;;      NIL
;; (X L R) - x is atom; l,r is node
(define nil (quote nil))
(define nil?
  (lambda (node)
    (eq? node nil)))
(define xof car)
(define lof cadr)
(define rof caddr)
(define istree
  (lambda (tree)
    (if (nil? tree) #t
      (if (not (atom? (xof tree))) #f
        (if (and [istree (lof tree)]
                 [istree (rof tree)]
                 [null? (cdddr tree)]) #t #f)))))

;; P55 (**) Construct completely balanced binary trees
;;     In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

;;     Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
;;     Example:
;;     * cbal-tree(4,T).
;;     T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
;;     T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
;;     etc......No
;; (cbal-tree N) -> all possible tree with N nodes
;; with 4 - 1 = 3, you can only have 1-2 or 2-1
;; with N - 1 = NN, you can only have (even)NN/2-NN/2, (odd) NN+1/2-NN-1/2
;; with 1, you can have 1-0 or 0-1, and that's covered in odd
;; result should be
;; (tree1 tree2 tree3 ...)
;; (cbal-tree 4) -> N = 3
;; (mark treeA treeB)
;; treeA -> (build (cb 2) (cb 1))
;; treeA -> (build (mark treeC treeD) (cb 1))
;; treeC -> (build (cb 1) (cb 0))
;; treeC -> (x (x nil nil) nil)
;; treeD -> (x nil (x nil nil))
;; (mark treeC treeD)
;; (build (build (cb 1) (cb 0)) (cb 1)) // more mark
;; only if I can return treeA first and return treeB after the whole thing 
(define build
  (lambda (L R)
    (let ([Aistree (istree L)]
          [Bistree (istree R)])
      (cond
       [(and Aistree Bistree)
        (cons 'x
              (cons L
                    (cons R
                          (quote ()))))]
       [else (if Aistree
                 (map (lambda (x)
                        (build L x)) R)
                 (map (lambda (x)
                        (build x R)) L))]))))
(define combine
  (lambda (a b)
    (cond
     [(or (not (istree a))
          (not (istree b)))
      (append a b)]
     [else
      (cons a
            (cons b (quote ())))])))

(define cbal-tree
  (lambda (N)
    (cond
     [(= N 0) nil]
     [(= N 1) (build nil nil)]
     [else (let ((N (- N 1)))
             (cond
              [(even? N)
               (let ((treeR (cbal-tree (/ N 2))))
                 (build treeR
                        treeR))]
              [else
               (let ([treeA [cbal-tree (/ (+ N 1) 2)]]
                     [treeB [cbal-tree (/ (- N 1) 2)]])
                 (combine (build treeA
                                 treeB)
                          (build treeB
                                 treeA)))]))])))
