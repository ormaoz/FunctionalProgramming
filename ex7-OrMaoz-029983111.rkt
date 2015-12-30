; Functional and Logical Programming 2014 - Excrecise 7 Solution 

; My name is Or Maoz
; My Id is 029983111

(require (lib "trace.rkt"))

; Global interpreter constants
(define CONTEXT_TYPE 'static) ; can be either 'static or 'dynamic
(define PRINT_MACRO_EXPANSION #f)

; Bonus constant - change to #t if you implement the bonus. Keep on #f otherwise.
(define LESS_THAN_ENABLED #f)

; ********************************************************************************************
; * Do not change anything in the code below, until where marked 'Your code should be here'. *
; * You may only change value of user-definitions if you do part 7.                          *
; ********************************************************************************************

; Special keywords - special forms that are implemented in the interpreter
(define special-keywords '(() #t #f lambda nlambda macro if eval apply))

; Primitive functions - functions that are used as primitives from the Dr. Racket interpreter
(define primitive-functions '(+ - * / < > <= >= <> = eq? equal? null? pair? cons car cdr))

; System context - contains default system functions (non-primitive) and constants - Can add more here
(define system-definitions '((pi 3.14159265358979)
                             (list (lambda x x))
                             (quote (nlambda (x) x))
                             (caar (macro (p) (list 'car (list 'car p))))
                             (cadr (macro (p) (list 'car (list 'cdr p))))
                             (cadar (macro (p) (list 'car (list 'cdr (list 'car p)))))
                             (cond-make-conds (lambda (conds)
                                                (if (null? conds)
                                                    ()
                                                    (if (eq? 'else (caar conds))
                                                        (cadar conds)
                                                        (list 'if (caar conds) (cadar conds)
                                                              (cond-make-conds (cdr conds)))))))
                             (cond (macro conds (cond-make-conds conds)))
                             (map (lambda (pred lst)
                                    (if (null? lst) ()
                                        (cons (pred (car lst)) (map pred (cdr lst))))))
                             (append (lambda (lst1 lst2)
                                       (if (null? lst1) lst2
                                           (cons (car lst1) (append (cdr lst1) lst2)))))
                             (let (macro (defs body) 
                                         (append (list (list 'lambda (map car defs) body))
                                                 (map cadr defs))))
                             ))

; User context - contains user functions (non-primitive) and constants - Can add more here
(define user-definitions '((first (macro (lst) (list 'car lst)))
                           (second (macro (lst) (list 'car (list 'cdr lst))))
                           (third (macro (lst) (list 'car (list 'cdr (list 'cdr lst)))))
                           (fourth (macro (lst) (list 'car (list 'cdr (list 'cdr (list 'cdr lst))))))
                           ; ***********************
                           ; * Add bonus code here *
                           ; ***********************
                           ))

; Makes a context out of a given list of definitions
(define (make-context dict)
  (if (null? dict) ()
      (dict-put (caar dict) (evaluate (cadar dict) ()) (make-context (cdr dict)))))

; Runs user code with an empty initial context
(define (run-code expr)
  (evaluate expr ()))

; Shows a prompt to the user to enter his code to run
(define (show-prompt-loop)
  (display "Enter an expression (type 'exit' to stop):")
  (newline)
  (let ((exp (read)))
    (if (not (eq? exp 'exit))
        (let ((result (run-code exp)))
          (if (not (eq? result (void)))
              (begin
                (display result)
                (newline)))
          (show-prompt-loop)))))

; Dictionary management (from class)
(define (dict-put key value ctx)
  (cons (list key value) ctx))

(define (dict-put-many entries ctx)
  (append entries ctx))

(define (dict-get key ctx)
  (let ((res (assoc key ctx)))
    (if res (cadr res) #f)))

; ***************************************************************************************
; ********************************* Add your code here! *********************************
; ***************************************************************************************


; Part 1: Evaluate Arguments
(define (eval-args args ctx)
  (map (lambda (x) (evaluate x ctx)) args))


; Part 2: Bind Parameters to Arguments
(define (bind params args)
  (define (helper params args result)
    (if (pair? params)
        (if (null? (cdr params))
            (append result (cons (list (car params) (car args)) ())) 
            (helper (cdr params) (cdr args) (append result (cons (list (car params) (car args)) ()))))
        (append result (list (list params args)))))
  (helper params args ()))

; Part 3: Evaluating a Symbol
(define (eval-symbol sym ctx)
  (cond ((member sym special-keywords) sym)
        ((member sym (map first ctx)) (dict-get sym ctx))
        ((member sym (map first user-context)) (dict-get sym user-context))
        ((member sym (map first system-context)) (dict-get sym system-context))
        ((member sym primitive-functions) (list '_primitive (eval sym)))))

; Part 4: Evaluating an if Expression
(define (eval-if condition if-true if-false ctx)
  (if (not (evaluate condition ctx))
      (evaluate if-false ctx)
      (evaluate if-true ctx)))

; Part 5: Evaluating a function call
; A.
(define (exec-func func args ctx)
  (if (eq? (car func) '_primitive)
      (apply (second func) (eval-args args ctx))
      (exec-user-func func args ctx)))

; B.
(define (exec-apply func args-list ctx)
  (evaluate (cons func (evaluate args-list ctx)) ctx))

; C.
(define (exec-user-func func args ctx)
  (let ((current_ctx  (if (eq? CONTEXT_TYPE 'static) ; Choose the right context (acording to the dynamic / static status)
                          (fourth func)
                          ctx)))
    
    ; The functino that prints macro expansions
    (define (macro_stepper)
      (if PRINT_MACRO_EXPANSION
          (begin (display "Macro expansion from:") 
                 (newline) 
                 (display (third func)) 
                 (newline)
                 (display "To:")
                 (newline)
                 (display (evaluate (third func) (dict-put-many (bind (second func) args) current_ctx)))
                 (newline) 
                 (newline))))
    
    ; Evaluate the body according to the type
    (cond ((eq? (car func) '_user_lambda) (evaluate (third func) (dict-put-many (bind (second func) (eval-args args ctx)) current_ctx)))
          ((eq? (car func) '_user_nlambda) (evaluate (third func) (dict-put-many (bind (second func) args) current_ctx)))
          ((eq? (car func) '_user_macro) 
           (begin (macro_stepper) (evaluate (evaluate (third func) (dict-put-many (bind (second func) args) current_ctx)) ctx))))))


; Part 6: Implementing the evaluate function
(define (evaluate exp ctx)
  (cond ((null? exp) ())
        ((symbol? exp) (eval-symbol exp ctx))
        ((not (list? exp)) exp)
        (else (let ((type (evaluate (first exp) ctx)))
                ; Call the right function with the right parameters according to the type of function
                (cond ((eq? type 'lambda) (list '_user_lambda (second exp) (third exp) ctx))
                      ((eq? type 'nlambda) (list '_user_nlambda (second exp) (third exp) ctx))
                      ((eq? type 'macro) (list '_user_macro (second exp) (third exp) ctx))
                      ((eq? type 'if) (eval-if (second exp) (third exp) (fourth exp) ctx))
                      ((eq? type 'eval) (evaluate (evaluate (second exp) ctx) ctx))
                      ((eq? type 'apply) (exec-apply (second exp) (third exp) ctx))
                      (else (exec-func type (cdr exp) ctx)))))))




; ***************************************************************************************
; *           The following lines should appear at the end, BELOW your code!            *
; *                            Do NOT change the code below                             *
; ***************************************************************************************

; Initially create system context
(define system-context (make-context system-definitions))

; Initially create user context
(define user-context (make-context user-definitions))






; *****************************************************************************************
; * Use the following code to test your code. Copy it to the end of your interpreter code *
; * file and run. If you see no error, all tests succeeded.                               *
; * NOTE: These tests are not enough to make sure that your code is 100% correct. This is *
; * only an infarstructure for you to create your own tests.                              *
; *****************************************************************************************

; *** DO NOT LEAVE TEST CODE IN YOUR SUBMITTED FILE!!! ***

; A macro that is used to test the behavior of the interpreter
(defmacro assert (expr result)
  `(if (not (equal? ,expr ,result))
       (error "Assertion failed: " (quote ,expr))))

