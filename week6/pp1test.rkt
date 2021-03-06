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

   ; sum-tree tests
   (check-equal?
    (sum-tree (btree-leaf))
    0
    "sum-tree 0 test")

   (check-equal?
    (sum-tree (btree-node 1
                          (btree-leaf)
                          (btree-leaf)))
    1
    "sum-tree 1 test")

   (check-equal?
    (sum-tree (btree-node 1
                          (btree-leaf)
                          (btree-node 2
                                      (btree-leaf)
                                      (btree-leaf))))
    3
    "sum-tree 3 test")

   (check-equal?
    (sum-tree (btree-node 1
                          (btree-node 3
                                      (btree-leaf)
                                      (btree-leaf))
                          (btree-node 2
                                      (btree-leaf)
                                      (btree-leaf))))
    6
    "sum-tree 6 test")

   (check-equal?
    (sum-tree (btree-node 1
                          (btree-node 2
                                      (btree-node 4
                                                  (btree-leaf)
                                                  (btree-leaf))
                                      (btree-node 5
                                                  (btree-leaf)
                                                  (btree-leaf)))
                          (btree-node 3
                                      (btree-node 6
                                                  (btree-leaf)
                                                  (btree-leaf))
                                      (btree-node 7
                                                  (btree-leaf)
                                                  (btree-leaf)))))
    28
    "sum-tree 28 test")

   ; prune-at-v tests
   (let ([tree (btree-node 1
                          (btree-node 2
                                      (btree-node 3
                                                  (btree-leaf)
                                                  (btree-leaf))
                                      (btree-node 4
                                                  (btree-leaf)
                                                  (btree-leaf)))
                          (btree-node 3
                                      (btree-node 5
                                                  (btree-leaf)
                                                  (btree-leaf))
                                      (btree-node 5
                                                  (btree-leaf)
                                                  (btree-leaf))))]
         [tree-pruned-at-1 (btree-leaf)]
         [tree-pruned-at-2 (btree-node 1
                                       (btree-leaf)
                                       (btree-node 3
                                                   (btree-node 5
                                                               (btree-leaf)
                                                               (btree-leaf))
                                                   (btree-node 5
                                                               (btree-leaf)
                                                               (btree-leaf))))]
         [tree-pruned-at-3 (btree-node 1
                                       (btree-node 2
                                                   (btree-leaf)
                                                   (btree-node 4
                                                               (btree-leaf)
                                                               (btree-leaf)))
                                       (btree-leaf))]
         [tree-pruned-at-4 (btree-node 1
                                       (btree-node 2
                                                   (btree-node 3
                                                               (btree-leaf)
                                                               (btree-leaf))                                                   
                                      (btree-leaf))
                          (btree-node 3
                                      (btree-node 5
                                                  (btree-leaf)
                                                  (btree-leaf))
                                      (btree-node 5
                                                  (btree-leaf)
                                                  (btree-leaf))))]
         [tree-pruned-at-5 (btree-node 1
                                       (btree-node 2
                                                   (btree-node 3
                                                               (btree-leaf)
                                                               (btree-leaf))
                                                   (btree-node 4
                                                               (btree-leaf)
                                                               (btree-leaf)))
                                       (btree-node 3
                                                   (btree-leaf)
                                                   (btree-leaf)))])
     
     (check-equal? (prune-at-v tree 1) tree-pruned-at-1 "prune-at-v 1 test")
     (check-equal? (prune-at-v tree 2) tree-pruned-at-2 "prune-at-v 2 test")
     (check-equal? (prune-at-v tree 3) tree-pruned-at-3 "prune-at-v 3 test")
     (check-equal? (prune-at-v tree 4) tree-pruned-at-4 "prune-at-v 4 test")
     (check-equal? (prune-at-v tree 5) tree-pruned-at-5 "prune-at-v 5 test"))

   ; well-formed-tree tests
   (check-equal? (well-formed-tree? #t) #f "#t is not a well-formed tree")
   (check-equal? (well-formed-tree? (btree-leaf)) #t "well-formed btree-leaf")
   (check-equal? (well-formed-tree? (btree-node #t #t #t)) #f "bad-formed btree-node")
   (check-equal? (well-formed-tree? (btree-node #t (btree-leaf) (btree-leaf))) #t "well-formed btree-node")
   (check-equal? (well-formed-tree?
                  (btree-node 1
                              (btree-node 2
                                          (btree-node 3
                                                      (btree-leaf)
                                                      (btree-leaf))
                                          (btree-node 4
                                                      (btree-leaf)
                                                      (btree-leaf)))
                              (btree-node 3
                                          (btree-node 5
                                                      (btree-leaf)
                                                      (btree-leaf))
                                          (btree-node 5
                                                      (btree-leaf)
                                                      #t))))
                 #f
                 "deeply nested bad-formed btree-node")

   ; fold-tree tests
   (check-equal? (fold-tree (lambda (x y) (+ x y 1))
                            99
                            (btree-leaf))
                 99
                 "fold-tree returns acc for btree-leaf")
   
   (check-equal? (fold-tree (lambda (x y) (+ x y 1))
                            7
                            (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf)))
                 18
                 "fold-tree folds btree-node test")

   (check-equal? (fold-tree (lambda (x y) (* x y))
                            7
                            (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf)))
                 140
                 "fold-tree folds btree-node test")

   ; curried-fold-tree tests
   (check-equal? ((((curried-fold-tree) (lambda (x y) (+ x y 1)))
                            99)
                            (btree-leaf))
                 99
                 "fold-tree returns acc for btree-leaf")))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)