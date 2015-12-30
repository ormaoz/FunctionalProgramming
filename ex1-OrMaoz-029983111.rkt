; Functional and Logical Programming 2014 - Excrecise 1 Solution 

; My name is Or Maoz
; My Id is 029983111

; Part 1
; A.
; I. ((300 + (8 * 9) + 11) / 8)
; II. ((5 - 7) * 8 * (8 * 9)) +  (33 / 11)
; III. (((((((7 * 6) * 8) * 5) * 4) * 3) * 2) * 1)

; B. 
; I. (+ 5 (+ (* 7 9) (* 8 2)))
; II. (* (+ 8 4 5) (/ (+ 9 7) (* 8 7 6)))
; III. (- 2 (/ (+ 4 (* 8 9)) 4))

; Part 2
; A. 
(define positiveEven 
  (lambda (x)
    (if (and (= (modulo x 2) 0) (> x 0))
        'yes
        'no)))

; B.
(define circle-circumference
  (lambda (r)
    (* 2 pi r)))

; Part 3
(define someSequence 
  (lambda (n)
    (if (= n 1)
        1
        (+ (someSequence (- n 1)) (expt 3 (- n 1))))))

; Part 4

; This function returns the n element in a Fibonacci series
(define fibonacci
  (lambda (n)
    (if (< n 3)
        1
        (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

; This function displays the whole Fibonacci series up to the n element using the "fibonacci" function
(define fibo
  (lambda (n)
    (if (< n 2)
        (begin
          (display 1)
          (newline))
        (begin
          (fibo (- n 1))
          (display (fibonacci n))
          (newline)))))



