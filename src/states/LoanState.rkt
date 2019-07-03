#lang eopl

(provide (all-defined-out))

(define-datatype LoanState LoanState?
  (a-loan-state
    (time integer?) ; Start
    (type integer?)
    (debt integer?)
  )
)

(define loan-state->time
  (lambda (loan-state)
    (cases LoanState loan-state
      (a-loan-state (time type debt)
        time
      )
    )
  )
)

(define loan-state->type
  (lambda (loan-state)
    (cases LoanState loan-state
      (a-loan-state (time type debt)
        type
      )
    )
  )
)

(define loan-state->debt
  (lambda (loan-state)
    (cases LoanState loan-state
      (a-loan-state (time type debt)
        debt
      )
    )
  )
)
