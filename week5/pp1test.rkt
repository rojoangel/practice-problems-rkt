#lang racket

(require "pp1.rkt")
(require rackunit)

(define tests
  (test-suite
   "Tests for practice problems in pp1.rkt"

   ; palindromic tests
   (check-equal? (palindromic null) null "palindromic null list test")
   (check-equal? (palindromic (list 1)) (list 2) "palindromic single elem list test")
   (check-equal? (palindromic (list 1 2)) (list 3 3) "palindromic 2 elems list test")
   (check-equal? (palindromic (list 1 2 4)) (list 5 4 5) "palindromic 3 elems list test")
   (check-equal? (palindromic (list 1 2 4 8)) (list 9 6 6 9) "palindromic 4 elems list test")

   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
