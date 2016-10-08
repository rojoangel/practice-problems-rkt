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

;; Write a function stream-until that takes a function f and a stream s, and applies
;; f to the values of s in succession until f evaluates to #f.
(define (stream-until f s)
  (let* ([stream (s)]
         [val (car stream)])
    (if (f val)
        (stream-until f (cdr stream))
        val)))

;; Write a function stream-map that takes a function f and a stream s, and returns
;; a new stream whose values are the result of applying f to the values produced by s.
(define (stream-map f s)
  (letrec ([aux (lambda (x) (cons (f (car (x))) (lambda () (aux (cdr (x))))))])
    (lambda () (aux s))))

;; Write a function stream-zip that takes in two streams s1 and s2 and returns a
;; stream that produces the pairs that result from the other two streams (so the first
;; value for the result stream will be the pair of the first value of s1 and the first
;; value of s2).
(define (stream-zip s1 s2)
  (letrec ([f (lambda (s1 s2) (cons (cons (car (s1)) (car (s2)))
                                    (lambda () (f (cdr (s1)) (cdr (s2))))))])
    (lambda () (f s1 s2))))

;; Write a function interleave that takes a list of streams and produces a new stream
;; that takes one element from each stream in sequence. So it will first produce the
;; first value of the first stream, then the first value of the second stream and so on,
;; and it will go back to the first stream when it reaches the end of the list.
;; Try to do this without ever adding an element to the end of a list.
(define (interleave xs)
  (letrec ([f (lambda (current-xs used-xs)
                (cons (car ((car used-xs)))
                      (if (null? (cdr used-xs))
                          (let ([next-xs (map (lambda (x) (cdr (x))) current-xs)]) 
                            (lambda () (f next-xs next-xs)))
                          (lambda () (f xs (cdr used-xs))))))])
    (lambda () (f xs xs))))

;; Define a function pack that takes an integer n and a stream s, and returns a stream
;; that produces the same values as s but packed in lists of n elements. So the first
;; value of the new stream will be the list consisting of the first n values of s, the
;; second value of the new stream will contain the next n values, and so on.
(define (pack n s)
  (letrec ([f (lambda (i l s)
                (if (< i n)
                    (f (+ i 1) (cons (car (s)) l) (cdr (s)))
                    (cons (reverse (cons (car (s)) l))
                          (lambda () (f 1 null (cdr (s)))))))])
    (lambda () (f 1 null s))))

;; We'll use Newton's Method for approximating the square root of a number, but by
;; producing a stream of ever-better approximations so that clients can "decide later"
;; how approximate a result they want: Write a function sqrt-stream that takes a number
;; n, starts with n as an initial guess in the stream, and produces successive guesses
;; applying fn(x)=1/2((x+n/x) to the current guess.
(define (sqrt-stream n)
  (letrec ([f (lambda (guess)
                (let ([next-guess (/ (+ guess (/ n guess)) 2)])
                  (cons next-guess 
                        (lambda () (f next-guess)))))])
    (lambda () (f n))))

;; Now use sqrt-stream from the previous problem to define a function approx-sqrt that
;; takes two numbers n and e and returns a number x such that x⋅x is within e of n.
;; Be sure not to create more than one stream nor ask for the same value from the
;; stream more than once. Note: Because Racket defaults to fully precise rational
;; values, you may wish to use a floating-point number for n (e.g., 10.0 instead of 10)
;; as well as for e.
(define (approx-sqrt n e)
  (letrec ([loop (lambda (stream)
                   (let ([approx (car (stream))])
                     (if (< (abs (- (* approx approx) n)) e)
                         approx
                         (loop (cdr (stream))))))])
    (loop (sqrt-stream n))))

;; Write a macro perform that has the following two forms:
;; (perform e1 if e2)
;; (perform e1 unless e2)
;; e1 should be evaluated (once) depending on the result of evaluating e2 -- only if
;; e2 evaluates to #f in the latter case, and only if it doesn't in the former case.
;; If e1 is never evaluated, the entire expression should evaluate to e2. Neither e1
;; nor e2 should be evaluated more than once in any case.
(define-syntax perform
  (syntax-rules (if unless)
    [(perform e1 if e2)
     (let ([pr e2])
       (if pr e1 pr))]
    [(perform e1 unless e2)
     (let ([pr e2])
       (if pr pr e1))]))
