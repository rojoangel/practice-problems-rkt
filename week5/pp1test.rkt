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

   ; fibonacci tests
   (check-equal? (car (fibonacci)) 0                  "fibonacci starts with 0 ..")
   (check-equal? (car ((cdr (fibonacci)))) 1              ".. continues with 1 ..")
   (check-equal? (car ((cdr ((cdr (fibonacci)))))) 1                 ".. and 1 ..")
   (check-equal? (car ((cdr ((cdr ((cdr (fibonacci)))))))) 2         ".. and 2 ..")
   (check-equal? (car ((cdr ((cdr ((cdr ((cdr (fibonacci)))))))))) 3 ".. and 3 ..")

   ; stream until tests
   (check-equal? (stream-until (lambda (x) (< x 0)) fibonacci) 0
                 "stream-until test using fibonacci")
   (check-equal? (stream-until (lambda (x) (< x 1)) fibonacci) 1
                 "stream-until test using fibonacci")
   (check-equal? (stream-until (lambda (x) (< x 30)) fibonacci) 34
                 "stream-until test using fibonacci")
   (check-equal? (stream-until (lambda (x) (< x 1000)) fibonacci) 1597
                 "stream-until test using fibonacci")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
