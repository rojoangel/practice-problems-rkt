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

;; Define a stream fibonacci, the first element of which is 0, the second one is 1,
;; and each successive element is the sum of two immediately preceding elements.
(define fibonacci
  (letrec([memo null] ; list of pairs (arg . result) 
          [fib (lambda (x)
               (let ([ans (assoc x memo)])
                 (if ans 
                     (cdr ans)
                     (let ([new-ans (cond [(= x 1) 0]
                                          [(= x 2) 1]
                                          [#t (+ (fib (- x 1))
                                                 (fib (- x 2)))])])
                       (begin 
                         (set! memo (cons (cons x new-ans) memo))
                         new-ans)))))]
          [f (lambda (x) (cons (fib x) (lambda () (f (+ x 1)))))])
    (lambda() (f 1))))
