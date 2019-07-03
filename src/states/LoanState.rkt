#lang eopl

(provide (all-defined-out))

(define-datatype LoanState LoanState?
  (a-loan-state
    (time integer?) ; Start month
    (type integer?)
    (debt integer?)
    (is-withdrawn boolean?)
  )
)

(define loan-state->time
  (lambda (loan-state)
    (cases LoanState loan-state
      (a-loan-state (time type debt is-withdrawn)
        time
      )
    )
  )
)

(define loan-state->type
  (lambda (loan-state)
    (cases LoanState loan-state
      (a-loan-state (time type debt is-withdrawn)
        type
      )
    )
  )
)

(define loan-state->debt
  (lambda (loan-state)
    (cases LoanState loan-state
      (a-loan-state (time type debt is-withdrawn)
        debt
      )
    )
  )
)

(define loan-state->is-withdrawn
  (lambda (loan-state)
    (cases LoanState loan-state
      (a-loan-state (time type debt is-withdrawn)
        is-withdrawn
      )
    )
  )
)
