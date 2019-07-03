#lang eopl

(provide (all-defined-out))

(define-datatype ToDo ToDo?
  (give-loan
    (amount integer?) 
  )
)