#lang racket

(require "pp2.rkt")
(require rackunit)

(define tests
  (test-suite
   "Tests for practice problems in pp2.rkt"

   ; crazy-sum tests
   (check-equal? (crazy-sum (list 10 * 6 / 5 - 3))
                 9 "crazy-sum test")
   (check-equal? (crazy-sum (list 1 2 3 4 5 6 7))
                 28 "crazy-sum sums if no function in list")
   (check-equal? (crazy-sum null)
                 0 "crazy-sum returns 0 for empty list")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)