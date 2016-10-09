#lang racket

(require "pp2.rkt")
(require "pp1.rkt") ; to make available binary tree structs
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

   ; either-fold tests
   (check-equal? (either-fold (lambda (x y) (+ x y)) 0 null)
                 0 "either-fold folds empty list")
   (check-equal? (either-fold (lambda (x y) (+ x y)) 0 (list 1 2 3))
                 6 "either-fold folds non empty list")
   (check-equal? (either-fold (lambda (x y) (+ x y)) 0 (btree-leaf))
                 0 "either-fold folds btree-leaf")
   (check-equal? (either-fold (lambda (x y) (+ x y)) 1 (btree-node 5
                                                                   (btree-leaf)
                                                                   (btree-node 7
                                                                               (btree-leaf)
                                                                               (btree-leaf))))
                 13 "either-fold folds btree-node")
   (check-exn exn:fail? (lambda()
                          (either-fold (lambda (x y) (+ x y)) 0 (vector null)))
              "either-fold raises error if not a list or btree")
   (check-exn exn:fail? (lambda()
                          (either-fold (lambda (x y) (+ x y)) 0 (btree-node 5 6 7)))
              "either-fold raises error if not a list or btree")

   ; flatten tests
   (check-equal? (flatten null)
                 null
                 "flatten flattens empty list")
   (check-equal? (flatten (list 1 2 3 4 5 6 7 8 9 10))
                 (list 1 2 3 4 5 6 7 8 9 10)
                 "flatten flattens flat list")
   (check-equal? (flatten (list 1 2 (list (list 3 4) 5 (list (list 6) 7 8)) 9 (list 10)))
                 (list 1 2 3 4 5 6 7 8 9 10)
                 "flatten flattens nested list")))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)