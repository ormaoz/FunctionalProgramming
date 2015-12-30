; Functional and Logical Programming 2014 - Excrecise 5 Solution 

; My name is Or Maoz
; My Id is 029983111

; Part 1 – Ranges macro
(defmacro ranges (x . cases)
  (define (expandCases case)
    (if (equal? (car case) 'else)
        case
        (if (pair? (car case))
            `((and (>= element ,(caar case)) (<= element ,@(cdar case))) ,(cadr case))
            `((= element ,(car case)) ,(cadr case)))))
  
  ; main macro code
  (if (null? cases)
      '(void)
      `(let ((element ,x))
         (cond ,@(map expandCases cases)))))

;Part 2 - Multi-dict, simple data abstraction
;A.
; The Multi-Dictionary will be a list of lists. 
; The big list will be the dictionary itself. 
; Then, Inside of it, there will be lists of keys and values in the form of (key . values). 
; So eventually we'll have a list of inner-lists where the inner-lists have a first element of "key" 
; and the second element will be a list of values.
; So the dictionary on the example, will look like this: ((a (1 2)) (b (3)) (c (4 5 6)))

;B.
(define (multidict-get key dict)
  (let ((key-values (filter (lambda (x) (eq? (car x) key)) dict)))
    (if (null? key-values)
        #f
        (cadar key-values))))

; For example: (multidict-get 'a '((a (1 2)) (b (3)) (c (4 5 6))))   ===>   (1 2)
;              (multidict-get 'g '((a (1 2)) (b (3)) (c (4 5 6))))   ===>    #f

;C.
(define (multidict-remove key dict)
  (filter (lambda (x) (not (eq? (car x) key))) dict))

; For example: (multidict-remove 'b '((a (1 2)) (b (3)) (c (4 5 6))))   ===>   ((a (1 2)) (c (4 5 6)))
;              (multidict-remove 'g '((a (1 2)) (b (3)) (c (4 5 6))))   ===>    ((a (1 2)) (b (3)) (c (4 5 6)))

;D.
(define (multidict-put key value dict)
  (define (isKey? key value car-dict) ; This helper function enters a new value to a key (will be used later with map)
    (if (eq? (car car-dict) key)
        (list key (append (cadr car-dict) (cons value ())))
        car-dict))
  (if (multidict-get key dict) ; If the key exist, use the isKey? function with map the enter the new value, else, put the new key and value at the end of the dictionary
      (map (lambda (x) (isKey? key value x)) dict)
      (append dict (list `(,key (,value))))))

; For example: (multidict-put 'a 7 '((a (1 2)) (b (3)) (c (4 5 6))))      ===>   ((a (1 2 7)) (b (3)) (c (4 5 6)))
;              (multidict-put 'a 2 '((a (1 2 7)) (b (3)) (c (4 5 6))))    ===>   ((a (1 2 7 2)) (b (3)) (c (4 5 6)))
;              (multidict-put 'd 7 '((a (1 2 7 2)) (b (3)) (c (4 5 6)))   ===>   ((a (1 2 7 2)) (b (3)) (c (4 5 6)) (d (7)))
