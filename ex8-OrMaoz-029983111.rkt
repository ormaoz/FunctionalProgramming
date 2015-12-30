; Functional and Logical Programming 2014 - Excrecise 8 Solution 

; My name is Or Maoz
; My Id is 029983111

; Part 1
;A. 
; Helper function to sum up elements in a list
(define (sum lst)
  (cond ((null? lst) 0)
        (else (+ (car lst) (sum (cdr lst))))))

; The actual bin pack function
(define (binPack1 items capacity)
  (define (helper items capacity one-bin)
    
    ; If we already packed all the items, returns the last bin to be packed (will be listed to the others)
    (cond ((null? items) (list one-bin))
          
          ; Checks if an additional item will exceed the capacity of the bin
          ((<= (+ (first items) (sum one-bin)) capacity)
           (helper (cdr items) capacity (append one-bin (list (first items))))) ; If it won't: pack it to that bin
          (else (append (list one-bin) (helper items capacity ()))))) ; If it will: list the current bin and "open a new one"
  (helper items capacity ()))

;B. The complexity is O(n).

; Part 2 -----------------------------------------------------------------------------------------------------------------------------------
;A. 
; The actual bin pack function
(define (binPack2 items capacity)
  ; Sort the items from large to small
  (let ((sorted-items (sort items >)))
    (define (helper items1 items2 capacity one-bin result)
      
      ; If we already packed all the items, pack the last bin to the result and return it
      (cond ((null? items1) (append result (list one-bin)))
            
            ; If we are over of options to add anything else to the current bin, "close" that bin, add it to the result and open a new bin
             ((null? items2) (helper items1 items1 capacity () (append result (list one-bin))))
            
            ; Checks if an additional item will exceed the capacity of the bin
            ((<= (+ (first items2) (sum one-bin)) capacity)
             ; If it's not exceeding: pack it to that current bin and remove it from the unpacked list (items1)
             (helper (remove (first items2) items1) (cdr items2) capacity (append one-bin (list (first items2))) result)) 
            ; If it is exceeding: try the next item in the list
            (else (helper items1 (cdr items2) capacity one-bin result))))
    
    ; Call the helper function with 2 copies of the items an "empty" bin and an "empty" result
    (helper sorted-items sorted-items capacity () ())))    

;B. The complexity is O(n^2).

; Part 3 --------------------------------------------------------------------------------------------------------------------------------

(define (binPack3 items capacity)
  ; Sort the items from large to small
  (let ((sorted-items (sort items >)))
    (define (helper items capacity old-bins bins)
      ; If we already packed all the items, pack the last bins to the rest and it return it
      (cond ((null? items) (append old-bins bins))
            ; If we are over of bins, put item in a new bin
            ((null? bins) (helper (cdr items) capacity () (append old-bins (list (list (first items))))))
            
            ; Checks if an additional item will exceed the capacity of the bin
            ((<= (+ (first items) (sum (first bins))) capacity)
             ; If it not exceeding:        
                    ; Try number 1: try to put the item in the current bin
             (let* ((bins-with-next (helper (cdr items) capacity () (append (list (append (list (first items)) (first bins))) (cdr bins) old-bins)))
                    ; Try number 2: try to put the item in the next bin
                    (bins-without-next (helper items capacity (append old-bins (list (first bins))) (cdr bins))))
               ; Check which try is better
               (if (< (length bins-with-next) (length bins-without-next))
                   bins-with-next
                   bins-without-next)))
            
            ; If it is exceeding: try the next bin
            (else (helper items capacity (append old-bins (list (first bins))) (cdr bins)))))
      
      ; Call the helper function with the items an empty bins and  empty old bins
      (helper sorted-items capacity () ())))


