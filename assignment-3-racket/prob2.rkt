#lang racket



;; ===================================== Preamble ======================================

(provide (all-defined-out)) ;; so we can put tests in a second file

;; Definition of structures for MF/PL programs - Do NOT change
(struct var (string) #:transparent) ;; a variable, e.g., (var "foo")
(struct int (num) #:transparent) ;; a constant number, e.g., (int 17)
(struct add (e1 e2) #:transparent) ;; add two expressions
(struct if> (e1 e2 e3 e4) #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual) #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2) #:transparent) ;; make a new pair
(struct fst (e) #:transparent) ;; get first part of a pair
(struct snd (e) #:transparent) ;; get second part of a pair
(struct aunit () #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)



;; ====================================== Answers ======================================

; 2.1: mfpl-list->rkt-list
; walk through the MF/PL list and build a Racket list
; aunit means the program hit the end, so return '()
; if the head is itself an apair or aunit, recursively convert it too
(define (mfpl-list->rkt-list lst)
  (cond
    [(aunit? lst) '()]
    [(apair? lst)
     (let ([head (apair-e1 lst)])
       (cons (if (or (apair? head) (aunit? head))
                 (mfpl-list->rkt-list head) 
                 head)
             (mfpl-list->rkt-list (apair-e2 lst))))]
    [else (error "mfpl-list->rkt-list: not a MF/PL list")]))

; 2.2: rkt-list->mfpl-list
; walk through the Racket list and build a MF/PL list
; empty list becomes aunit
; if the head is a nested list, convert it recursively
(define (rkt-list->mfpl-list lst)
  (cond
    [(null? lst) (aunit)]
    [else
     (let ([head (car lst)])
       (apair (if (list? head)
                  (rkt-list->mfpl-list head) ; nested list, convert it too
                  head)
              (rkt-list->mfpl-list (cdr lst))))]))

; 2.3
;; Lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond
    [(null? env) (error "unbound variable during evaluation" str)]
    [(equal? (car (car env)) str) (cdr (car env))]
    [#t (envlookup (cdr env) str)]))
; TODO:
;; DO NOT change the two cases given to you.
;; DO add more cases for other kinds of MF/PL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp (see below).
(define (eval-under-env e env)
  (cond
    [(var? e) (envlookup env (var-string e))]
    [(add? e)
     (let ([v1 (eval-under-env (add-e1 e) env)] [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1) (int? v2))
           (int (+ (int-num v1) (int-num v2)))
           (error "MF/PL addition applied to non-number")))]

    ; values just return themselves
    [(int? e) e]
    [(aunit? e) e]
    [(closure? e) e]

    ; a function becomes a closure that captures the current environment
    [(fun? e) (closure env e)]

    ; evaluate both sides, make sure they're ints, then pick the right branch
    [(if>? e)
     (let ([v1 (eval-under-env (if>-e1 e) env)]
           [v2 (eval-under-env (if>-e2 e) env)])
       (if (and (int? v1) (int? v2))
           (if (> (int-num v1) (int-num v2))
               (eval-under-env (if>-e3 e) env)
               (eval-under-env (if>-e4 e) env))
           (error "MF/PL if> applied to non-number")))]

    ; evaluate e, add the binding to the front of the environment, then eval body
    [(mlet? e)
     (let ([v (eval-under-env (mlet-e e) env)])
       (eval-under-env (mlet-body e)
                       (cons (cons (mlet-var e) v) env)))]

    ; evaluate the function and argument
    ; the function must be a closure - run its body in the closure's saved environment
    ; extend that env with the function name (for recursion) and the parameter
    [(call? e)
     (let ([vfun (eval-under-env (call-funexp e) env)]
           [varg (eval-under-env (call-actual e) env)])
       (if (closure? vfun)
           (let* ([f          (closure-fun vfun)]
                  [clo-env    (closure-env vfun)]
                  [env+arg    (cons (cons (fun-formal f) varg) clo-env)]
                  ; only add the function name binding if it's not anonymous (#f)
                  [env+fn+arg (if (fun-nameopt f)
                                  (cons (cons (fun-nameopt f) vfun) env+arg)
                                  env+arg)])
             (eval-under-env (fun-body f) env+fn+arg))
           (error "MF/PL call applied to non-function")))]

    ; evaluate both sides and make a new pair
    [(apair? e)
     (apair (eval-under-env (apair-e1 e) env)
            (eval-under-env (apair-e2 e) env))]

    ; evaluate the expression, make sure it's a pair, then return first element
    [(fst? e)
     (let ([v (eval-under-env (fst-e e) env)])
       (if (apair? v)
           (apair-e1 v)
           (error "MF/PL fst applied to non-pair")))]

    ; same as fst but return second element
    [(snd? e)
     (let ([v (eval-under-env (snd-e e) env)])
       (if (apair? v)
           (apair-e2 v)
           (error "MF/PL snd applied to non-pair")))]

    ; return 1 if it's aunit, 0 otherwise
    [(isaunit? e)
     (let ([v (eval-under-env (isaunit-e e) env)])
       (if (aunit? v) (int 1) (int 0)))]

    [#t (error (format "bad MF/PL expression: ~v" e))]))

;; Do NOT change (modify eval-under-env instead!)
;; Note how evaluating an expression start with an empty environment
(define (eval-exp e)
  (eval-under-env e null))

; 2.4: ifaunit
; if e1 is aunit then evaluate e2, otherwise evaluate e3
; isaunit gives us 1 for aunit and 0 otherwise
; so we just check if isaunit(e1) > 0
(define (ifaunit e1 e2 e3)
  (if> (isaunit e1) (int 0) e2 e3))

; 2.5: mlet*
; like let* in Racket - bind each variable in order so later ones can use earlier ones
; base case: no more bindings, just return the body expression
; recursive case: wrap in one mlet, then handle the rest
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst))       ; the variable name
            (cdr (car lstlst))       ; the expression to bind it to
            (mlet* (cdr lstlst) e2))))

; 2.6: if=
; check if e1 equals e2 - if so evaluate e3, otherwise e4
; we can't do == directly, but we can use if>
; x == y means NOT (x > y) AND NOT (y > x)
(define (if= e1 e2 e3 e4)
  (mlet* (list (cons "x" e1) (cons "y" e2))
         (if> (var "x") (var "y")
              e4                     ; x > y, so not equal
              (if> (var "y") (var "x")
                   e4                ; y > x, so not equal
                   e3))))            ; neither, so they must be equal

; 2.7: mfpl-map
; curried map for MF/PL: takes f first, returns a function that takes a list
; if the list is empty (aunit), return aunit
; otherwise apply f to the head, then recurse on the tail
(define mfpl-map
  (fun "mfpl-map" "f"           ; named so it can call itself recursively
       (fun #f "lst"             ; returns a function that takes the list
            (if> (isaunit (var "lst"))
                 (int 0)         ; isaunit returns 1 for aunit, and 1 > 0 so we go here
                 (aunit)         ; empty list case
                 ; non-empty: apply f to head, recurse on tail
                 (apair (call (var "f") (fst (var "lst")))
                        (call (call (var "mfpl-map") (var "f"))
                              (snd (var "lst"))))))))

; 2.8: mfpl-map-add-N
; takes an integer i, returns a function that adds i to every element in a list
; bring mfpl-map into MF/PL scope using mlet, then use it to map an add function
(define mfpl-map-add-N
  (mlet "map" mfpl-map           ; bind mfpl-map so we can use it inside
        (fun #f "i"              ; function that takes i
             (call (var "map")   
                   (fun #f "x"   
                        (add (var "x") (var "i")))))))



;; ==================================== Test suite =====================================
; See prob2_test.rkt