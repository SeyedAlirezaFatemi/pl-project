#lang racket


(provide (all-defined-out))
(require eopl)
(require "../states/LoanState.rkt")

(define latest-loan-time
  (lambda (loan-states)
    (if (null? loan-states)
      #f
      (let ([time (loan-state->time (car loan-states))])
        (if (null? (cdr loan-states))
          time
          (let ([rest-time (latest-loan-time (cdr loan-states))])
            (if (> time rest-time)
              time
              (if rest-time
                rest-time
                0
              )
            )
          )
        )
      )
    )
  )
)

(define latest-loan
  (lambda (loan-states)
    (if (null? loan-states)
      #f
      (let ([time (loan-state->time (car loan-states))])
        (if (null? (cdr loan-states))
          (car loan-states)
          (let ([rest-time (latest-loan-time (cdr loan-states))])
            (if (> time rest-time)
              (car loan-states)
              (if (latest-loan (cdr loan-states))
                (latest-loan (cdr loan-states))
                0
              )
            )
          )
        )
      )
    )
  )
)


(define time->debt
  (lambda (spec-time loans)
    (if (null? loans)
      0
      (cases LoanState (car loans)
        (a-loan-state (time type debt is-withdrawn)
          (if (equal? spec-time time)
            debt
            (time->debt spec-time (cdr loans))
          )
        )
      )
    )
  )
)
