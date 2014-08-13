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

;============================
; Adding the grammar
; <tree> ::=  empty 
;           | number <tree> <tree>
;============================

;creates an empty-tree
(define (empty-tree)
  '()
)

;returns #t or #f if the tree is empty or not
(define (empty-tree? tree)
   (if (equal? (empty-tree) tree)
      #t
      #f
   )  
)

;creates a tree with only one node
(define (bstree root)
  (list root '() '()) ; Okay I know that procedural is all special, but look.  One line of code.  Beautiful.
)

;returns the root of the given tree
(define (tree->root tree)
  (car tree)  
)

;returns the left subtree
(define (tree->left tree)
  (car (cdr tree))
)

;returns the right subtree
(define (tree->right tree)
  (car (cdr (cdr tree)))
)

;inserts a node with value n into the tree while still maintaining
;the binary tree's properties mentioned above.
(define (insert-tree n t)
  (cond 
    ((empty-tree? t) (bstree n))
    ((> n (tree->root t)) (list (car t) (car (cdr t)) (insert-tree n (car (cdr (cdr t))))))
    (else (list (car t) (insert-tree n (car (cdr t))) (car (cdr (cdr t)))))
  )  
)

;return a list containing all the values of the tree in sorted order
(define (tree->list tree)
  (append
    (append
      (treetolist (cdr (tree->left tree)) (list (car (tree->left tree))))
      (list(tree->root tree)))
    (treetolist (cdr (tree->right tree)) (list (car (tree->right tree)))))
)

;helper method
(define (treetolist t f)
  (cond 
    ((null? t) f)
    ((null? (car t)) (treetolist (cdr t) f))
    (else (append (list (car t) f)))
  )
)

