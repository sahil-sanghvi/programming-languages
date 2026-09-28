#lang racket

(require rackunit "prob2.rkt")
(require rackunit/text-ui)

;; ==================================== Test suite =====================================

;; this macro is an exception handler
;; it executes the expression s
;; if s raises an exception it returns the string "this is an invalid..."
(define-syntax-rule (handler s)
  (with-handlers ([(lambda (v) #t)
                   (lambda (v) "this is an invalid value. probably due to an exception")])
    s))

(define test-warm-up
  (test-suite "Warm-up tests"
   (test-equal? "MF/PL list -> Racket list #1"
                (handler (mfpl-list->rkt-list
                          (apair (int 3) (apair (int 4) (apair (int 9) (aunit))))))
                (list (int 3) (int 4) (int 9)))
   (test-equal? "MF/PL list -> Racket list #2"
                (handler (mfpl-list->rkt-list
                          (apair (apair (int 42) (apair (var "x") (aunit)))
                                 (apair (apair (int 43) (apair (var "y") (aunit))) (aunit)))))
                (list (list (int 42) (var "x")) (list (int 43) (var "y"))))
   (test-equal? "MF/PL list -> Racket List #3 (empty)" (handler (mfpl-list->rkt-list (aunit))) '())
   (test-equal? "MF/PL List to Rackets List tests"
                (handler (mfpl-list->rkt-list
                          (apair (int 1) (apair (int 2) (apair (int 3) (apair (int 4) (aunit)))))))
                (list (int 1) (int 2) (int 3) (int 4)))
   (test-equal? "Racket list #1 to MF/PL list"
                (handler (rkt-list->mfpl-list (list (int 3) (int 4) (int 9))))
                (apair (int 3) (apair (int 4) (apair (int 9) (aunit)))))
   (test-equal? "Racket list #2 to MF/PL list"
                (handler (rkt-list->mfpl-list (list (list (int 42) (var "x"))
                                                    (list (int 43) (var "y")))))
                (apair (apair (int 42) (apair (var "x") (aunit)))
                       (apair (apair (int 43) (apair (var "y") (aunit))) (aunit))))
   (test-equal? "Racket list -> MF/PL list #3"
                (handler (rkt-list->mfpl-list (list (var "foo") (int 17))))
                (apair (var "foo") (apair (int 17) (aunit))))
   (test-equal? "Racket list -> MPL list #4 (with sublists)"
                (handler (rkt-list->mfpl-list (list (int 1) (int 2) (int 3) (int 4))))
                (apair (int 1) (apair (int 2) (apair (int 3) (apair (int 4) (aunit))))))
   (test-equal? "Racket list -> MPL list #5 (empty list)"
                (handler (rkt-list->mfpl-list '()))
                (aunit))))

(define test-eval
  (test-suite "Evaluation tests"
   (test-equal? "Simple int" (handler (eval-exp (int 5))) (int 5))
   (test-equal? "Simple addition" (handler (eval-exp (add (int 10) (int 7)))) (int 17))
   (test-equal? "Simple if>" (handler (eval-exp (if> (int 10) (int 5) (int 1) (int 0)))) (int 1))
   (test-equal? "Simple if> 2" (handler (eval-exp (if> (int 0) (int 5) (int 1) (int 0)))) (int 0))
   (test-equal? "Simple fst" (handler (eval-exp (fst (apair (int 1) (int 2))))) (int 1))
   (test-equal? "Simple snd" (handler (eval-exp (snd (apair (int 1) (int 2))))) (int 2))
   (test-equal? "Simple isaunit true" (handler (eval-exp (isaunit (aunit)))) (int 1))
   (test-equal? "Simple isaunit false" (handler (eval-exp (isaunit (int 10)))) (int 0))
   (test-equal? "aunit evaluation" (handler (eval-exp (aunit))) (aunit))
   (test-equal? "simple apair" (handler (eval-exp (apair (int 1) (aunit)))) (apair (int 1) (aunit)))
   (test-equal? "defining a variable using mlet"
                (handler (eval-exp (mlet "x" (int 5) (var "x"))))
                (int 5))
   (test-equal? "basic function definition anonymous"
                (handler (eval-exp (fun #f "x" (int 5))))
                (closure '() (fun #f "x" (int 5))))
   (test-equal? "basic function definition 2"
                (handler (eval-exp (fun "incr" "x" (add (var "x") (int 1)))))
                (closure '() (fun "incr" "x" (add (var "x") (int 1)))))
   (test-equal? "function call not using the parameter"
                (handler (eval-exp (call (fun #f "x" (int 1)) (aunit))))
                (int 1))
   (test-equal? "function call" (handler (eval-exp (call (fun #f "x" (var "x")) (int 5)))) (int 5))
   (test-equal? "function call 2"
                (handler (eval-exp (call (fun "incr" "x" (add (var "x") (int 1))) (int 42))))
                (int 43))
   (test-equal? "function call 3: make sure the parameter is evaluated"
                (handler (eval-exp (call (fun #f "x" (var "x")) (fst (apair (int 5) (int 3))))))
                (int 5))
   (test-equal? "function call 4: make sure the parameter is evaluated"
                (handler (eval-exp (call (fun #f "x" (fst (var "x"))) (apair (int 5) (int 3)))))
                (int 5))
   (test-equal? "function call 4: make sure the parameter is evaluated"
                (handler (eval-exp (call (fun #f "x" (var "x")) (apair (int 5) (int 3)))))
                (apair (int 5) (int 3)))
   (test-equal? "more complex mlet expressions"
                (handler (eval-exp (mlet "x" (int 5) (mlet "x" (add (var "x") (int 1)) (var "x")))))
                (int 6))
   (test-equal? "if> with invalid e4"
                (handler (eval-exp (if> (add (int 2) (int 2))
                                        (add (int 2) (int 1))
                                        (add (int 3) (int -3))
                                        (add "wrong" "bad"))))
                (int 0))
   (test-equal? "apair should reduce its contents"
                (handler (eval-exp (apair (fst (apair (int 3) (int 10))) (int 4))))
                (apair (int 3) (int 4)))
   (test-equal? "fst/snd test"
                (handler (eval-exp (apair (fst (apair (int 1) (int 2)))
                                          (snd (apair (int 3) (int 4))))))
                (apair (int 1) (int 4)))
   (test-equal? "fst should reduce its expression before returning"
                (handler (eval-exp (fst (apair (add (int 1) (int 2)) (int 3)))))
                (int 3))
   (test-equal? "fst should reduce the expression before testing"
                (handler (eval-exp (fst (snd (apair (int 1) (apair (int 2) (int 3)))))))
                (int 2))
   (test-equal? "snd should reduce the expression before testing"
                (handler (eval-exp (snd (snd (apair (int 1) (apair (int 2) (int 3)))))))
                (int 3))
   (test-equal? "snd should reduce its expression before returning"
                (handler (eval-exp (snd (apair (add (int 1) (int 2)) (int 4)))))
                (int 4))
   (test-equal? "make sure isaunit evaluates its parameter"
                (handler (eval-exp (isaunit (snd (apair (int 1) (aunit))))))
                (int 1))
   (test-equal? "basic scope with call"
                (handler (eval-exp (call (mlet "x" (int 5) (fun "test" "x" (var "x"))) (int 100))))
                (int 100))
   (test-equal? "basic scope with call 2"
                (handler (eval-exp (call (mlet "x" (int 5) (fun "test" "x" (add (int 1) (var "x"))))
                                         (int 100))))
                (int 101))
   (test-equal? "test call of previously defined function (call test 15)"
                (handler (eval-exp (mlet "test" (fun #f "b" (var "b")) (call (var "test") (int 15)))))
                (int 15))
   (test-equal?
    "Local scoping. If you get 1730 you have used the wrong environment for the function call"
    (handler (eval-exp (mlet "f1"
                             (fun #f "a" (mlet "x" (var "a") (fun #f "z" (add (var "x") (int 1)))))
                             (mlet "f3"
                                   (fun #f "f" (mlet "x" (int 1729) (call (var "f") (aunit))))
                                   (call (var "f3") (call (var "f1") (int 1)))))))
    (int 2))
   (test-equal? "Sum over list: test of recursive function"
                (handler (eval-exp (mlet "fnc"
                                         (fun "f1"
                                              "x"
                                              (if> (isaunit (var "x"))
                                                   (int 0)
                                                   (int 0)
                                                   (add (fst (var "x"))
                                                        (call (var "f1") (snd (var "x"))))))
                                         (call (var "fnc") (apair (int 1) (aunit))))))
                (int 1))
   (test-equal?
    "Sum over list: test of recursive function 2"
    (handler (eval-exp (mlet "fnc"
                             (fun "f1"
                                  "x"
                                  (if> (isaunit (var "x"))
                                       (int 0)
                                       (int 0)
                                       (add (fst (var "x")) (call (var "f1") (snd (var "x"))))))
                             (call (var "fnc")
                                   (apair (int 1) (apair (int 2) (apair (int 3) (aunit))))))))
    (int 6))
   (test-equal? "complex add"
                (handler (eval-exp (add (add (int 1) (int 2)) (add (int 3) (int 4)))))
                (int 10))
   (test-equal? "mlet and var 1"
                (handler (eval-exp (mlet "x" (add (int 1) (int 1)) (var "x"))))
                (int 2))
   (test-equal? "mlet and var 2" (handler (eval-exp (mlet "x" (int 1) (var "x")))) (int 1))
   (test-equal? "fun evaluation"
                (handler (eval-exp (fun #f "x" (var "x"))))
                (closure '() (fun #f "x" (var "x"))))
   (test-equal? "mlet and fun evaluation"
                (handler (eval-exp (mlet "x" (int 1) (fun #f "a" (var "x")))))
                (closure (list (cons "x" (int 1))) (fun #f "a" (var "x"))))
   (test-equal? "complex if>, false"
                (handler (eval-exp (if> (add (int 0) (int 1)) (add (int 0) (int 2)) (int 3) (int 4))))
                (int 4))
   (test-equal? "complex if>, false 2"
                (handler (eval-exp (if> (int 1) (int 2) (int 3) (add (int 2) (int 2)))))
                (int 4))
   (test-equal? "complex if greater, true"
                (handler (eval-exp (if> (add (int 0) (int 2)) (add (int 1) (int 0)) (int 3) (int 4))))
                (int 3))
   (test-equal? "complex if>, true 2"
                (handler (eval-exp (if> (int 2) (int 1) (add (int 1) (int 2)) (int 4))))
                (int 3))
   (test-equal? "int apair" (handler (eval-exp (apair (int 1) (int 1)))) (apair (int 1) (int 1)))
   (test-equal? "var apair"
                (handler (eval-exp (mlet "x" (int 1) (apair (var "x") (var "x")))))
                (apair (int 1) (int 1)))
   (test-equal? "mlet and fst"
                (handler (eval-exp (mlet "x" (apair (int 1) (int 2)) (fst (var "x")))))
                (int 1))
   (test-equal? "snd evaluation" (handler (eval-exp (snd (apair (int 1) (int 2))))) (int 2))
   (test-equal? "mlet and snd"
                (handler (eval-exp (mlet "x" (apair (int 1) (int 2)) (snd (var "x")))))
                (int 2))
   (test-equal? "mlet isaunit true"
                (handler (eval-exp (mlet "x" (aunit) (isaunit (var "x")))))
                (int 1))
   (test-equal? "mlet isaunit false"
                (handler (eval-exp (mlet "x" (int 0) (isaunit (var "x")))))
                (int 0))
   (test-equal? "double function, non-recursive."
                (handler (eval-exp (mlet "double"
                                         (fun "double" "x" (add (var "x") (var "x")))
                                         (call (var "double") (int 10)))))
                (int 20))
   (test-equal?
    "range function, recursive. generates a list of int from 5 to 8. Make sure call env is: function, parm, env, in that order"
    (handler (eval-exp (mlet "range"
                             (fun "range"
                                  "lo"
                                  (fun #f
                                       "hi"
                                       (if> (var "lo")
                                            (var "hi")
                                            (aunit)
                                            (apair (var "lo")
                                                   (call (call (var "range") (add (int 1) (var "lo")))
                                                         (var "hi"))))))
                             (call (call (var "range") (int 5)) (int 8)))))
    (apair (int 5) (apair (int 6) (apair (int 7) (apair (int 8) (aunit))))))
   (test-case "add exception" (check-exn #rx"MF/PL" (lambda () (eval-exp (add (int 3) (aunit))))))
   (test-case "var exception" (check-exn #rx"unbound" (lambda () (eval-exp (var "x")))))
   (test-case "if> exception"
              (check-exn #rx"MF/PL" (lambda () (eval-exp (if> "1" (int 2) (int 3) (int 4))))))
   (test-case "fst exception"
              (check-exn #rx"MF/PL" (lambda () (eval-exp (fst (add (int 1) (int 2)))))))
   (test-case "snd exception"
              (check-exn #rx"MF/PL" (lambda () (eval-exp (snd (add (int 1) (int 2)))))))
   (test-case "call exception" (check-exn #rx"MF/PL" (lambda () (eval-exp (call (int 1) (int 2))))))
   (test-case "bad expression exception"
              (check-exn #rx"MF/PL" (lambda () (eval-exp (list (int 1) (int 2))))))))

(define test-expand
  (test-suite "Expansion tests"
   (test-equal? "ifaunit test #1" (handler (eval-exp (ifaunit (aunit) (int 2) (int 3)))) (int 2))
   (test-equal? "ifaunit test #2" (handler (eval-exp (ifaunit (int 3) (int 2) (int 3)))) (int 3))
   (test-equal? "ifaunit true"
                (handler (eval-exp (ifaunit (aunit) (add (int 1) (int 2)) (add (int 3) (int 4)))))
                (int 3))
   (test-equal? "ifaunit false"
                (handler (eval-exp (ifaunit (int 0) (add (int 1) (int 2)) (add (int 3) (int 4)))))
                (int 7))
   (test-equal? "mlet* basic test"
                (handler (eval-exp (mlet* (cons (cons "x" (int 1)) null) (var "x"))))
                (int 1))
   (test-equal? "mlet* basic test again "
                (handler (eval-exp (mlet* (list (cons "x" (int 1))) (var "x"))))
                (int 1))
   (test-equal? "mlet a bit more complicated"
                (handler (eval-exp (mlet* (list (cons "f" (int 2)) (cons "y" (int 15)))
                                          (add (var "f") (add (var "y") (int 3))))))
                (int 20))
   (test-equal? "normal mlet* evaluation"
                (handler (eval-exp (mlet* (list (cons "x" (int 1)) (cons "y" (int 2)))
                                          (add (var "x") (var "y")))))
                (int 3))
   (test-equal? "single variable mlet* evaluation"
                (handler (eval-exp (mlet* (list (cons "x" (int 1))) (var "x"))))
                (int 1))
   (test-equal? "shadowing mlet* evaluation"
                (handler (eval-exp (mlet* (list (cons "x" (int 1)) (cons "x" (int 2))) (var "x"))))
                (int 2))
   (test-equal? "simple if= true evaluation"
                (handler (eval-exp (if= (int 1) (int 1) (int 2) (int 3))))
                (int 2))
   (test-equal? "simple if= false evaluation"
                (handler (eval-exp (if= (int 0) (int 1) (int 2) (int 3))))
                (int 3))
   (test-equal? "simple if= false evaluation"
                (handler (eval-exp (if= (int 1) (int 0) (int 2) (int 3))))
                (int 3))
   (test-equal? "complex if= true evaluation"
                (handler (eval-exp (if= (add (int 1) (int 1)) (int 2) (int 2) (int 3))))
                (int 2))
   (test-equal? "complex if= false evaluation"
                (handler (eval-exp (if= (add (int 1) (int 1)) (int 1) (int 2) (int 3))))
                (int 3))
   (test-equal? "another if="
                (handler (eval-exp (if= (int 2) (add (int 1) (int 1)) (int 1) (int 2))))
                (int 1))))

(define test-use
  (test-suite "Usage tests"
   (test-case "mfpl-map"
              (define addtwo (fun "addone" "x" (add (var "x") (int 2))))
              (define mfpl-map-addtwo (call mfpl-map addtwo))
              (check-equal? (handler (eval-exp (call mfpl-map-addtwo (aunit)))) (aunit))
              (define my-mfpl-list (apair (int 23) (apair (int 42) (aunit))))
              (define my-answers (apair (int 25) (apair (int 44) (aunit))))
              (check-equal? (handler (eval-exp (call mfpl-map-addtwo my-mfpl-list))) my-answers))
   (test-case
    "mfpl-map"
    (check-equal? (handler (eval-exp (call (call mfpl-map (fun #f "x" (add (int 1) (var "x"))))
                                           (apair (int 1) (apair (int 2) (aunit))))))
                  (apair (int 2) (apair (int 3) (aunit)))
                  "map normal list")
    (check-equal? (handler (eval-exp (call (call mfpl-map (fun #f "x" (add (int 1) (var "x"))))
                                           (apair (int 1) (aunit)))))
                  (apair (int 2) (aunit))
                  "map single item list")
    (check-equal?
     (handler (eval-exp (call (call mfpl-map (fun #f "x" (add (int 1) (var "x")))) (aunit))))
     (aunit)
     "map empty list"))
   (test-case "mfpl-map-add-N"
              (define input (apair (int 25) (apair (int 44) (aunit))))
              (define output (apair (int 26) (apair (int 45) (aunit))))
              (check-equal? (handler (eval-exp (call (call mfpl-map-add-N (int 1)) input))) output))
   (test-case "mfpl-map-add-N 2"
              (check-equal? (handler (eval-exp (call (call mfpl-map-add-N (int 7))
                                                     (rkt-list->mfpl-list '()))))
                            (aunit)
                            "map-add-N empty list"))
   (test-case "mfpl-map-add-N 3"
              (check-equal? (handler (eval-exp (call (call mfpl-map-add-N (int 7))
                                                     (rkt-list->mfpl-list
                                                      (list (int 3) (int 4) (int 9))))))
                            (rkt-list->mfpl-list (list (int 10) (int 11) (int 16)))
                            "map-add-N +7"))
   (test-case "mfpl-map-add-N 4"
              (check-equal? (handler (eval-exp (call (call mfpl-map-add-N (int 7))
                                                     (rkt-list->mfpl-list (list (int 3))))))
                            (rkt-list->mfpl-list (list (int 10)))
                            "map-add-N single item list"))))

(run-tests test-warm-up)
(run-tests test-eval)
(run-tests test-expand)
(run-tests test-use)
