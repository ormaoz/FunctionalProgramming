; Functional and Logical Programming 2014 - Excrecise 3 Solution 

; My name is Or Maoz
; My Id is 029983111

;Part 1
(define quick-sort (lambda (pred lst)
                     
                     ; This function returns the pivot (first element in the list)
                     (define pivot (lambda (lst)
                                     (cond ((or (null? lst) (null? (cdr lst)))
                                            'done)
                                           (else 
                                            (car lst)))))
                     
                     ; This function devides the lists into two lists according to condition
                     (define partition (lambda (pred piv lst)
                                         
                                         ; These two functions define the conditions based on the predicate given by the user
                                         (define doesPred? (lambda (x)
                                                             (if (pred x piv)
                                                                 #t
                                                                 #f )))
                                       
                                         (define notPred? (lambda (x)
                                                            (if (not (pred x piv))
                                                                #t
                                                                #f )))
                                         
                                         ; Now I'll remove the pivot and create the partitions according to the conditions
                                         (define lstNoPiv (remove piv lst))
                                         (append (list (filter doesPred? lstNoPiv)) (list piv) (list (filter notPred? lstNoPiv)))))
                     
                     ; The function that runs it all
                     (let ((piv (pivot lst)))
                       (if (equal? piv 'done) 
                           lst
                           (let ((parts (partition pred piv lst)))
                             (append 
                              (quick-sort pred (first parts))
                              (cons piv ())
                              (quick-sort pred (third parts))))))))

;Part 2
;A.
(define do2add (lambda (lst)
                 (define helper (lambda (lst result)
                                  (if (null? lst)
                                      result
                                      (append result (cons (+ (first lst) (second lst)) ()) (helper (cddr lst) result)))))
                 (helper lst ())))

;B.
(define do2F (lambda (F lst)
               (define helper (lambda (F lst result)
                                (if (null? lst)
                                    result
                                    (append result (cons (F (first lst) (second lst)) ()) (helper F (cddr lst) result)))))
               (helper F lst ())))
;C.
(define makedo2F (lambda (F)
                   (lambda (n)
                     (do2F F n))))
;D.
(define do2addFactory (makedo2F (lambda (x y) (+ x y))))
(define do2mult (makedo2F (lambda (x y) (* x y))))
(define do2eq? (makedo2F (lambda (x y) (equal? x y))))
(define do2eq1st (makedo2F (lambda (x y) (equal? (car x) (car y)))))

;Part 3
(define makeDo2FM (lambda (F)
                    (lambda (x . y)
                      (do2F F (append (cons x ()) y)))))

;(define do2multM (makedo2FM (lambda (x y) (* x y))))
; WORKS  :-)
