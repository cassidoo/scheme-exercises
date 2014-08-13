#lang racket

;============================
; Checks if parenthesis are balanced
;============================

(define (balanced? in)
  (define inlst (string->list in))
  (if (equal? (string-length in) 0)
      #t
      (balcount inlst 0)
  )
)

(define (balcount lst count)
  (if (< count 0) ; for the case ")("
      #f
      (cond
        ((and (= count 0) (= (length lst) 0)) #t)
        ((and (not(= count 0)) (= (length lst) 0)) #f)
        ((equal? (string (list-ref lst 0)) "(") (balcount (list-tail lst 1) (+ count 1)))
        ((equal? (string (list-ref lst 0)) ")") (balcount (list-tail lst 1) (- count 1)))
        (else (balcount (list-tail lst 1) count))
      )
  )
)