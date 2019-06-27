#lang eopl

; (require "src/blueprints/AccountBlueprint.rkt")
; (require "src/blueprints/LoanBlueprint.rkt")

(define-datatype Customer Customer?
  (an-account-state
    (id integer?)
    (type integer?)
    (initial-amount integer?)
    (amount integer?)
    (deadline-month integer?)
    (credit-counter integer?)
    (credit integer?)
    (interest-rate integer?)
    (loans (listof ))
  )
)
