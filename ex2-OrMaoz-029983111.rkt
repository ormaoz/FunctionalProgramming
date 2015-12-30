; Functional and Logical Programming 2014 - Excrecise 2 Solution 

; My name is Or Maoz
; My Id is 029983111

; Part 1
; A.
(define (multListByN n lst)
  (if (null? lst)
      ()
      (cons 
       (* n (car lst)) 
       (multListByN n (cdr lst)))))

;B.
(define (multListByList lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      ()
      (cons 
       (multListByN (car lst1) lst2) 
       (multListByList (cdr lst1) lst2))))

;C.

(define (multListsAny lst1 lst2)
  (cond ((= (length lst1) (length lst2)) ; In case the list are the same length: just multiply and call again with the rest of the list
         (if (null? lst1)
             ()
             (cons 
              (* (car lst1) (car lst2)) 
              (multListsAny (cdr lst1) (cdr lst2)))))
        ((> (length lst1) (length lst2)) ; In case one of the lists is longer, use the chainer function
         (if (or (null? (cdr lst1)) (null? lst2))
             ()
             (chainer lst1 lst2)))
        (else
         (if (or (null? (cdr lst2)) (null? lst1))
             ()
             (chainer lst2 lst1)))))

; An helper function that chaining together: the product of two cons and
; recursively call the function with the rest of the list when the shorter one 
; is chained in the end with its previews head
(define (chainer lst1 lst2)
  (cons 
   (* (car lst1) (car lst2))
   (multListsAny (cdr lst1) (append (cdr lst2) (cons (car lst2) ())))))

; Part 2
;A. 

(define (quick-sort lst)
  (define half (ceiling (/ (length lst) 2)))
  ; how to use: (pivot [some list])
  ; returns the middle element of the list or 'done if the list is empty / has one element
  (define (pivot lst)
    (cond ((null? lst) 'done)
          ((null? (cdr lst)) 'done)
          (else 
           (cond ((<= (length lst) half)
                  (car lst))
                 (else
                  (pivot (cdr lst)))))))
  
  ; how to use: (partition [pivot element] [list with element to sort] () ())
  ; returns two lists which are the partitions based on the pivot
  (define (partition piv lst p1 p2)
    (if (null? lst) 
        (list p1 p2)
        (cond ((< (car lst) piv) 
               (partition piv (cdr lst) (cons (car lst) p1) p2))
              ((> (car lst) piv)
               (partition piv (cdr lst) p1 (cons (car lst) p2)))
              ((= (car lst) piv)
               (partition piv (cdr lst) (append p1 (cons (car lst) ())) p2)))))
  
  (let ((piv (pivot lst)))
    (if (equal? piv 'done) 
        lst
        (let ((parts (partition piv lst () ())))
          (append (quick-sort (car parts)) 
                  (quick-sort (cadr parts)))))))
;B.I
(define (merge l1 l2)
  (cond ((and (null? l1) ( null? l2)) ())
        ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2))
         (cons (car l1) (merge (cdr l1) l2)))
        (else
         (cons (car l2) (merge l1 (cdr l2))))))

(define (split lst)
  (define (helper lst lister)
    (if (null? lst)
        lister
        (helper (cdr lst) (append lister (list (cons (car lst) ()))))))
  (helper lst ()))

(define (chainer lst)
  (if (or (null? lst)(= (length lst) 1))
      lst
      (cons (merge (car lst)(second lst))(chainer (cddr lst)))))

(define (fasterMergeSort lst)
  (define (helper lst)
    (cond ((= ( length lst) 1)
           (car lst))
          (else
           (helper (chainer lst)))))
  (helper (split lst)))

;B.II
; O(n log n) - n for spliting plus nlogn for merging. Total: O(nlogn)
; Over all it is not different than the regular merge sort which is also O(nlogn).

;Part 3
;A. 
(define (sumNums n)
  (define (helper n r)
    (if (= n 0)
        r
        (helper (- n 1) (+ n r))))
  (helper n 0))

;B.
(define (numOfBitsOn number)
  (define (helper number times)
    (cond ((= number 1)
           times)
          ((= number 0)
           0)
          (else
           (helper (floor (/ number 2)) (+ times (modulo number 2))))))
  (helper number 1))
