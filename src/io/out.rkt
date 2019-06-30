#lang eopl

(require "../states/Customer.rkt")
(require "../states/LoanState.rkt")
(provide (all-defined-out))

(define finish
  (lambda (last-month customers)
    (let ([out (open-output-file "result.txt")])
      (write last-month out)
      (newline out)
      (write-customers customers out)
      (close-output-port out)
    )
  ) 
)

(define write-customers
  (lambda (customers out)
    (if (null? customers) 
      out
      (cases Customer (car customers)
          (a-customer (id type initial-amount amount deadline-month credit-counter credit interest-rate loans minimum-amount blocked-money)
            (write id out)
            (write initial-amount out)
            (write amount out)
            (write deadline-month out)
            (write credit out)
            (write interest-rate out)
            (write (time->debt (latest-loan-time loans) loans) out)
            (write (latest-loan-time loans) out)
            (write blocked-money out)
            ; loan deadline time
            (write (sum-of-debts loans) out)
            (write-customers (cdr customers) out)
          )
      )
    )
  )
)

(define sum-of-debts
  (lambda (loans debts)
    (if (null? loans)
      debts
      (cases LoanState (car loans)
        (a-loan-state (time type debt)
          (sum-of-debts (cdr loans) (+ debt debts))
        )
      )
    )
  )
)

(define latest-loan-time
  (lambda (loans)
    (if (null? loans)
      0
      (cases LoanState (car loans)
        (a-loan-state (time type debt)
          (if (null? (cdr loans))
            time
            (if (> time (latest-loan-time (cdr loans)))
              time
              (latest-loan-time (cdr loans))
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
        (a-loan-state (time type debt)
          (if (equal? spec-time time)
            debt
            (time->debt spec-time (cdr loans))
          )
        )
      )
    )
  )
)
