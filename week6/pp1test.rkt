#lang racket

(require "pp1.rkt")
(require rackunit)

(define tests
  (test-suite
   "Tests for practice problems in pp1.rkt"

   ; tree-height tests
   (check-equal?
    (tree-height (btree-leaf))
    0
    "tree-height 0 test")

   (check-equal?
    (tree-height (btree-node #t
                             (btree-leaf)
                             (btree-leaf)))
    1
    "tree-height 1 test")

   (check-equal?
    (tree-height (btree-node #t
                             (btree-leaf)
                             (btree-node #t
                                         (btree-leaf)
                                         (btree-leaf))))
    2
    "tree-height 2 test")

   (check-equal?
    (tree-height (btree-node #t
                             (btree-node #t
                                         (btree-node #t
                                                     (btree-leaf)
                                                     (btree-leaf))
                                         (btree-node #t
                                                     (btree-leaf)
                                                     (btree-leaf)))
                             (btree-node #t
                                         (btree-node #t
                                                     (btree-leaf)
                                                     (btree-leaf))
                                         (btree-node #t
                                                     (btree-leaf)
                                                     (btree-leaf)))))
    3
    "tree-height 3 test")

   (check-equal?
    (tree-height (btree-node #t
                             (btree-leaf)
                             (btree-node #t
                                         (btree-leaf)
                                         (btree-node #t
                                                     (btree-leaf)
                                                     (btree-node #t
                                                                 (btree-leaf)
                                                                 (btree-leaf))))))
    4
    "tree-height 4 test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)