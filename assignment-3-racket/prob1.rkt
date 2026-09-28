#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file


;; ====================================== Answers ======================================

; 1.1: stream-for-n-steps
; a stream is a thunk - when you call it you get back (value . next-thunk)
; base case: if n is 0, return empty list
; otherwise call the stream, grab the value, and recurse on the next thunk with n-1
(define (stream-for-n-steps s n)
  (if (= n 0)
      '()
      (let ([result (s)])              
        (cons (car result)
              (stream-for-n-steps (cdr result) (- n 1))))))

; 1.2: fibo-stream
; keep track of the last two fib numbers (a and b)
; each step gets a, then moves to (b, a+b)
; start with a=0, b=1 to get 0 1 1 2 3 5 8 ...
(define fibo-stream
  (letrec ([make-fibo (lambda (a b)
                        (lambda ()
                          (cons a (make-fibo b (+ a b)))))])
    (make-fibo 0 1)))

; 1.3: filter-stream

(define (filter-stream pred? s)
  (lambda ()
    (let loop ([current (s)])          ; call stream to get (value . next-thunk)
      (if (pred? (car current))
          (cons (car current)
                (filter-stream pred? (cdr current))) ; found one, wrap the rest
          (loop ((cdr current)))))))   ; skip this one, try the next

; 1.4: create-stream macro

(define-syntax create-stream
  (syntax-rules (using starting at with increment)
    [(create-stream name using f starting at i0 with increment delta)
     (define name
       (letrec ([make-stream
                 (lambda (i)
                   (lambda ()
                     (cons (f i) (make-stream (+ i delta)))))])
         
         (lambda ()
           (let ([start i0])
             ((make-stream start))))))]))



;; ==================================== Test suite =====================================

(require rackunit)

;; Sample stream for testing stream-for-n-steps
(define nat-num-stream (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))]) (lambda () (f 0))))

;; Test create-stream macro
(create-stream squares using (lambda (x) (* x x)) starting at 5 with increment 2)

(define tests
  (test-suite "Sample tests for A3 P1"
   (check-equal? (stream-for-n-steps nat-num-stream 10)
                 '(0 1 2 3 4 5 6 7 8 9)
                 "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps fibo-stream 10) 
                 '(0 1 1 2 3 5 8 13 21 34) 
                 "fibo-stream test")
   (check-equal? (stream-for-n-steps (filter-stream (lambda (i) (> i 5)) nat-num-stream) 5)
                 '(6 7 8 9 10)
                 "filter stream test")
   (check-equal? (stream-for-n-steps squares 5)
                 '(25 49 81 121 169)
                 "stream defined using a macro. only tests is return value")))

;; Run the tests
(require rackunit/text-ui)
(run-tests tests)