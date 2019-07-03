#lang racket

(provide (all-defined-out))
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
              rest-time
            )
          )
        )
      )
    )
  )
)
