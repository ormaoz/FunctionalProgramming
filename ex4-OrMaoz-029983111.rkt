; Functional and Logical Programming 2014 - Excrecise 4 Solution 

; My name is Or Maoz
; My Id is 029983111

; Part 1 – Simple Macro
;A.
(defmacro circle-circumference (r)
  `(* 2 pi ,r))

;B.
(defmacro max (x y)
  `(let ((a ,x) (b ,y))  
     (if (> a b)
         a
         b)))

;Part 2 – Less Simple Macro
(defmacro less-than (x . rules)
  
  ; helper function
  (define (expandRules rule)
    (if (equal? (car rule) 'else)
        rule
        `((< element ,(car rule)) ,(cadr rule))))
  
  ; main macro code
  (if (null? rules)
      '(void)
      `(let ((element ,x))
         (cond ,@(map expandRules rules)))))

;Part 3 – Boolean Logic
;A.

(defmacro nor (x . y)
  (define (expand args)
    (if (null? args) 
        #t
        `(if ,(car args) 
             #f 
             ,(expand (cdr args)))))
  (expand (cons x y)))

;B.
(defmacro xor (x . y)
   (define (expand args boolean) ; "boolean" keeps track of the boolean status of the exprassion as it's being expanded
    (if (null? args) 
        boolean
        `(if ,(car args) 
             ,(expand (cdr args) (not boolean)) ; In case the argument was true "change" boolean to its opposite
             ,(expand (cdr args) boolean)))) ; In case the argument was false, there is no change in the number of 
                                             ; true arguments and therefore no need to "change" boolean
  (expand (cons x y) #f))