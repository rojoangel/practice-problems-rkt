#lang racket

(provide (all-defined-out)) ;; so we can put our tests in a second file

;; Write a function palindromic that takes a list of numbers and evaluates to a list
;; of numbers of the same length, where each element is obtained as follows: the first
;; element should be the sum of the first and the last elements of the original list,
;; the second one should be the sum of the second and second to last elements of the
;; original list, etc.
(define (palindromic xs)
  (map (lambda (i j) (+ i j))
       (reverse xs)
       xs))