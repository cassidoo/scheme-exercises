#lang racket

; Procedural-based approach implementing a file data-type interface
; <file> ::= "ext-dir" <file>  <file>       "extend-directory"
;          | "empty-dir" string            "empty-directory"
;          | "regular" string number       "regular-file"

(define (invalid-args-msg fun-name-as-string
                          expected-value-type-as-predicate-string
                          received)
  (string-append "Invalid arguments in: " fun-name-as-string " --- "
                 "expected: " expected-value-type-as-predicate-string " --- "
                 "received: " (to-string received)
                 )
)

(define-datatype file file?
  (empty-directory (name string?))
  (regular-file (name string?) (bytes number?))
  (extend-directory (f1 file?) (f2 file?))
)

(define (regular-file? f)
  (cases file f
    (empty-directory (name) #f)
    (regular-file (name bytes) #t)
    (extend-directory (f1 f2) #f)
    (else (raise "what the heck"))
  )
)

(define (empty-directory? f)
  (cases file f
    (empty-directory (name) #t)
    (regular-file (name bytes) #f)
    (extend-directory (f1 f2) #f)
    (else (raise "what the heck"))
  )
)

(define (extend-directory? f)
  (cases file f
    (empty-directory (name) #f)
    (regular-file (name bytes) #f)
    (extend-directory (f1 f2) #t)
    (else (raise "what the heck"))
  )
)

(define (regular-file->name f)
  (cases file f
    (empty-directory (name) (invalid-args-msg "regular-file->name"
                                             "regular-file?"
                                             "empty-directory"))
    (regular-file (name bytes) name)
    (extend-directory (f1 f2) (invalid-args-msg "regular-file->name"
                                             "regular-file?"
                                             "extend-directory"))
    (else (raise "what the heck"))
  )
)

(define (regular-file->bytes f)
  (cases file f
    (empty-directory (name) (invalid-args-msg "regular-file->bytes"
                                             "regular-file?"
                                             "empty-directory"))
    (regular-file (name bytes) bytes)
    (extend-directory (f1 f2) (invalid-args-msg "regular-file->bytes"
                                             "regular-file?"
                                             "extend-directory"))
    (else (raise "what the heck"))
  )
)

(define (empty-directory->name f)
  (cases file f
    (empty-directory (name) name)
    (regular-file (name bytes) (invalid-args-msg "dir-file->name"
                                             "empty-directory?"
                                             "regular-file"))
    (extend-directory (f1 f2) (invalid-args-msg "dir-file->name"
                                             "empty-directory?"
                                             "extend-directory"))
    (else (raise "what the heck"))
  )
)


(define (extend-directory->f-1 f)
  (cases file f
    (empty-directory (name) (invalid-args-msg "extend-dir->f-1"
                                             "extend-dir?"
                                             "empty-dir"))
    (regular-file (name bytes) (invalid-args-msg "extend-dir->f-1"
                                             "extend-dir?"
                                             "regular-file"))
    (extend-directory (f1 f2) f1)
    (else (raise "what the heck"))
  )
)


(define (extend-directory->f-2 f)
  (cases file f
    (empty-directory (name) (invalid-args-msg "extend-dir->f-2"
                                             "extend-dir?"
                                             "empty-dir"))
    (regular-file (name bytes) (invalid-args-msg "extend-dir->f-2"
                                             "extend-dir?"
                                             "regular-file"))
    (extend-directory (f1 f2) f2)
    (else (raise "what the heck"))
  )
)

; Size of the file
(define (total-size file1)
  (cases file file1
    (empty-directory (name) 0)
    (regular-file (name bytes) bytes)
    (extend-directory (f1 f2) (+ (total-size f1) (total-size f2)))
    (else (raise "what the heck"))
  )
)