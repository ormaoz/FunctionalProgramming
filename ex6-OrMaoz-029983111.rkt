; Functional and Logical Programming 2014 - Excrecise 6 Solution 

; My name is Or Maoz
; My Id is 029983111

; Macros and functions for stream creation
(defmacro stream-cons (value next-expr)
  `(cons ,value (lambda () ,next-expr)))

(define stream-car car)

(define (stream-cdr stream)
  (if (procedure? (cdr stream))
      ((cdr stream))
      (cdr stream)))

(define (streamToList stream n)
  (if (= n 0)
      ()
      (cons 
       (stream-car stream)
       (streamToList (stream-cdr stream) (- n 1)))))

; Part 1 – Simple Streams
(define (generate-powers-of-2-stream)
  (define (helper x)
    (stream-cons x (helper (* 2 x))))
  (helper 2))


(define (generate-fibo)
  (define (helper a b) 
    (stream-cons a (helper b (+ a b))))
  (helper 1 1))

;Part 2 - List to Stream
(define (list-to-infinite-stream lst)
  (define (helper list)
    (stream-cons (car list) (helper (append (cdr list) (cons (car list) ())))))
  (helper lst))

;Part 3 – Stream comprehensions

; The regular stream-map from recitation (I'm using it in my function)
(define (stream-map pred . streams)
  (define (helper pred streams)
    (stream-cons (apply pred (map stream-car streams))
                 (helper pred (map stream-cdr streams))))
  (helper pred streams))

; My function 
(define (stream-comp pred baseStream . conditions)
  ; An helper function that checks all the condtions on one element (from the stream)
  (define (helper element conds)
    (if (null? conds)
        #t
        (if ((car conds) element)
            (helper element (cdr conds)) 
            #f)))
  
  ; This is the main body of my function: Checks if the current element
  ; from the stream returns true with all the conditions (using helper). if it does, add it 
  ; to the new stream, otherwise, run the function again with the rest of the stream.
  (define (helper2 baseStream conditions)
    (if (helper (stream-car baseStream) conditions)
        (stream-cons (stream-car baseStream)
                     (helper2 (stream-cdr baseStream) conditions))
        (helper2 (stream-cdr baseStream) conditions)))
  (stream-map pred (helper2 baseStream conditions)))  ; In the end, use the stream-map function to carry out the pred for the new stream
  