#lang eopl

(require "../states/Customer.rkt")
(require "../blueprints/Account.rkt")
(require "../states/LoanState.rkt")
(require "../commander/commander.rkt")
(provide (all-defined-out))

(define finish
  (lambda (last-month account-types customers)
    (let ([out (open-output-file "result.txt" #:exists 'replace)])
      (write last-month out)
      (newline out)
      (write-customers account-types customers out)
      (close-output-port out)
    )
  ) 
)

(define write-customers
  (lambda (account-types customers out)
    (if (null? customers) 
      out
      (cases Customer (car customers)
          (a-customer (id type initial-amount amount deadline-month credit-counter credit interest-rate loans minimum-amount blocked-money)
            (write id out)
            (write initial-amount out)
            (write amount out)
            (write deadline-month out)
            (write credit out)
            (if (has-interest? (get-account-type type account-types))
              (write 0 out)
              (write interest-rate out)
            )
            (write (time->debt (latest-loan-time loans) loans) out)
            (write (latest-loan-time loans) out)
            (write blocked-money out)
            ; loan deadline time
            (write (sum-of-debts loans 0) out)
            (write-customers account-types (cdr customers) out)
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
            (let ([rest-time (latest-loan-time (cdr loans))])
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
