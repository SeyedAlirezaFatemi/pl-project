#lang eopl

(provide (all-defined-out))
(require "../states/Customer.rkt")

(define-datatype ToDo ToDo?
  (give-loan
    (customer Customer?)
    (amount integer?) 
  )
)