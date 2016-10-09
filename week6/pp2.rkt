#lang racket

(require "pp1.rkt") ; to make available fold-tree to be used in either-fold

(provide (all-defined-out))

; Write a function crazy-sum that takes a list of numbers and adds them all together,
; starting from the left. There's a twist, however. The list is allowed to contain
; functions in addition to numbers. Whenever an element of a list is a function, you
; should start using it to combine all the following numbers in a list instead of +.
; You may assume that the list is non-empty and contains only numbers and binary
; functions suitable for operating on two numbers. Further assume the first list
; element is a number. For example, (crazy-sum (list 10 * 6 / 5 - 3)) evaluates to 9.
; Note: It may superficially look like the function implements infix syntax for
; arithmetic expressions, but that's not really the case.
(define (crazy-sum xs)
  (letrec ([f (lambda (acc op xs)
                (cond [(null? xs) acc]
                      [(number? (car xs)) (f (op acc (car xs)) op (cdr xs))]
                      [#t (f acc (car xs) (cdr xs))]))])
    (f 0 + xs)))

; Write a function either-fold that is like fold for lists or binary trees as defined
; above except that it works for both of them. Give an appropriate error message if
; the third argument to either-fold is neither a list nor a binary tree.
(define (either-fold binary-function acc list-or-tree)
  (cond [(list? list-or-tree) (foldl binary-function acc list-or-tree)]
        [(well-formed-tree? list-or-tree) (fold-tree binary-function acc list-or-tree)]
        [#t (error "not a list or tree")]))