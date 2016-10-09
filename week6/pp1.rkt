#lang racket

(provide (all-defined-out))

; provided definitions
(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

; Write a function tree-height that accepts a binary tree and evaluates to a height of
; this tree. The height of a tree is the length of the longest path to a leaf.
; Thus the height of a leaf is 0.
(define (tree-height t)
  (if (btree-leaf? t)
      0
      (let ([left-height (tree-height (btree-node-left t))]
                  [right-height (tree-height (btree-node-right t))])
              (+ 1
                 (if (> left-height right-height)
                     left-height
                     right-height)))))

; Write a function sum-tree that takes binary tree and sums all the values in all the
; nodes. (Assume the value fields all hold numbers, i.e., values that you can pass to
; +).
(define (sum-tree t)
  (if (btree-leaf? t)
      0
      (let ([left-sum (sum-tree (btree-node-left t))]
                  [right-sum (sum-tree (btree-node-right t))])
              (+ (btree-node-value t) (+ left-sum right-sum)))))

; Write a function prune-at-v that takes a binary tree t and a value v and produces a
; new binary tree with structure the same as t except any node with value equal to v
; (use Racket's equal?) is replaced (along with all its descendants) by a leaf.
(define (prune-at-v t v)
  (cond [(btree-leaf? t) t]
        [(equal? (btree-node-value t) v) (btree-leaf)]
        [#t (btree-node (btree-node-value t)
                     (prune-at-v (btree-node-left t) v)
                     (prune-at-v (btree-node-right t) v))]))

; Write a function well-formed-tree? that takes any value and returns #t if and only if
; the value is legal binary tree as defined above.
(define (well-formed-tree? t)
  (cond [(btree-leaf? t) #t]
        [(and (btree-node? t)
              (well-formed-tree? (btree-node-left t))
              (well-formed-tree? (btree-node-right t))) #t]
        [#t #f]))

; Write a function fold-tree that takes a two-argument function, an initial accumulator,
; and a binary tree and implements a fold over the tree, applying the function to all
; the values. For example,
; (fold-tree (lambda (x y) (+ x y 1)) 7
;   (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf)))
; would evaluate to 18. You can traverse the tree in any order you like (though it does
; affect the result of a call to fold-tree if the function passed isn't associative).
(define (fold-tree f acc t)
  (if (btree-leaf? t)
      acc
      (fold-tree f (fold-tree f (f acc (btree-node-value t)) (btree-node-left t)) (btree-node-right t))))