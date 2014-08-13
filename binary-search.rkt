#lang racket

;============================
; Binary search.  Self explanatory.
;============================

(define (binary-search lst elem)
  (define mid (floor (/ (length lst) 2)))
  (define midelem (list-ref lst mid))
  
  (if (null? lst)
     #f
     (cond
       ((and (= (length lst) 1) (not (equal? (first lst) elem))) #f)
       ((= midelem elem) #t)
       ((> midelem elem) (binary-search (take lst mid) elem))
       ((< midelem elem) (binary-search (drop lst mid) elem))
       (else #f)
     )
   )
)