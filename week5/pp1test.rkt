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

   ; stream-map tests
   (check-equal? (stream-until (lambda (x) (< x 0))
                               (stream-map (lambda (x) (* x 2)) fibonacci))
                 0
                 "stream-map test using stream-until & fibonacci")
   (check-equal? (stream-until (lambda (x) (< x 2))
                               (stream-map (lambda (x) (* x 2)) fibonacci))
                 2
                 "stream-map test using stream-until & fibonacci")
   (check-equal? (stream-until (lambda (x) (< x 60))
                               (stream-map (lambda (x) (* x 2)) fibonacci))
                 68
                 "stream-map test using stream-until & fibonacci")
   (check-equal? (stream-until (lambda (x) (< x 2000))
                               (stream-map (lambda (x) (* x 2)) fibonacci))
                 3194
                 "stream-map test using stream-until & fibonacci")

   ; stream-zip tests
   ;; define fibx2 and fibx3 as helpers
   (let* ([fibx2 (stream-map (lambda (x) (* x 2)) fibonacci)]
         [fibx3 (stream-map (lambda (x) (* x 3)) fibonacci)]
         [zip (stream-zip fibx2 fibx3)])
     (check-equal? (car (zip)) (cons 0 0)                          "test zip starts with (0 . 0)")
     (check-equal? (car ((cdr (zip)))) (cons 2 3)                  "continues with  with (2 . 3)")
     (check-equal? (car ((cdr ((cdr (zip)))))) (cons 2 3)          "continues with  with (2 . 3)")
     (check-equal? (car (( cdr ((cdr ((cdr (zip)))))))) (cons 4 6) "continues with  with (4 . 6)"))
   
   ; interleave tests
   (let* ([fibx2 (stream-map (lambda (x) (* x 2)) fibonacci)]
         [fibx3 (stream-map (lambda (x) (* x 3)) fibonacci)]
         [interleaved (interleave (list fibonacci fibx2 fibx3))])
     (check-equal? (car (interleaved)) 0                           "test interleaved starts with 0")
     (check-equal? (car ((cdr (interleaved)))) 0                                 "continues with 0")
     (check-equal? (car ((cdr ((cdr (interleaved)))))) 0                         "continues with 0")
     (check-equal? (car ((cdr ((cdr ((cdr (interleaved)))))))) 1                 "continues with 1")
     (check-equal? (car ((cdr ((cdr ((cdr ((cdr (interleaved)))))))))) 2         "continues with 2")
     (check-equal? (car ((cdr ((cdr ((cdr ((cdr ((cdr (interleaved)))))))))))) 3 "continues with 3"))

   ; pack tests
   (let ([packed-fib (pack 2 fibonacci)])
     (check-equal? (car (packed-fib)) (list 0 1)                                   "test pack using fib")
     (check-equal? (car ((cdr (packed-fib)))) (list 1 2)                           "test pack using fib")
     (check-equal? (car ((cdr ((cdr (packed-fib)))))) (list 3 5)                   "test pack using fib")
     (check-equal? (car ((cdr ((cdr ((cdr (packed-fib)))))))) (list 8 13)          "test pack using fib")
     (check-equal? (car ((cdr ((cdr ((cdr ((cdr (packed-fib)))))))))) (list 21 34) "test pack using fib"))

   ; sqrt-stream tests
   (let ([sqrt-10 (sqrt-stream 10.0)])
     (check-equal? (car (sqrt-10)) 5.5                                               "test sqrt-stream aprox 1")
     (check-equal? (car ((cdr (sqrt-10)))) 3.659090909090909                         "test sqrt-stream aprox 2")
     (check-equal? (car ((cdr ((cdr (sqrt-10)))))) 3.196005081874647                 "test sqrt-stream aprox 3")
     (check-equal? (car ((cdr ((cdr ((cdr (sqrt-10)))))))) 3.16245562280389          "test sqrt-stream aprox 4")
     (check-equal? (car ((cdr ((cdr ((cdr ((cdr (sqrt-10)))))))))) 3.162277665175675 "test sqrt-stream aprox 5"))

   ; approx-sqrt tests 
   (check-= 100.0 (* (approx-sqrt 100.0 10.0) (approx-sqrt 100.0 10.0)) 10.0         "test approx-sqrt 100.0 10.0")
   (check-= 100.0 (* (approx-sqrt 100.0 1.0) (approx-sqrt 100.0 1.0)) 1.0             "test approx-sqrt 100.0 1.0")
   (check-= 100.0 (* (approx-sqrt 100.0 0.1) (approx-sqrt 100.0 0.1)) 0.1             "test approx-sqrt 100.0 0.1")
   (check-= 100.0 (* (approx-sqrt 100.0 0.01) (approx-sqrt 100.0 0.01)) 0.01         "test approx-sqrt 100.0 0.01")   
   (check-= 100.0 (* (approx-sqrt 100.0 0.0001) (approx-sqrt 100.0 0.0001)) 0.0001 "test approx-sqrt 100.0 0.0001")   

   ; perform tests
   (check-equal? (perform "a" if #t) "a" "test perform")
   (check-equal? (perform "a" if #f) #f "test perform")
   (check-equal? (perform "a" unless #f) "a" "test perform")
   (check-equal? (perform "a" unless #t) #t "test perform")

   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
