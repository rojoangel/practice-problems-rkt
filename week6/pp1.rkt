#lang racket

(provide (all-defined-out))

; provided definitions
(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

; Write a function tree-height that accepts a binary tree and evaluates to a height of
; this tree. The height of a tree is the length of the longest path to a leaf.
; Thus the height of a leaf is 0.
(define (tree-height t)
  (cond [(btree-leaf? t) 0]
        [#t (let ([left-height (tree-height (btree-node-left t))]
                  [right-height (tree-height (btree-node-right t))])
              (+ 1
                 (if (> left-height right-height)
                     left-height
                     right-height)))]))

; Write a function sum-tree that takes binary tree and sums all the values in all the
; nodes. (Assume the value fields all hold numbers, i.e., values that you can pass to
; +).
(define (sum-tree t)
  (cond [(btree-leaf? t) 0]
        [#t (let ([left-sum (sum-tree (btree-node-left t))]
                  [right-sum (sum-tree (btree-node-right t))])
              (+ (btree-node-value t) (+ left-sum right-sum)))]))