(load "lists.scm")
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
