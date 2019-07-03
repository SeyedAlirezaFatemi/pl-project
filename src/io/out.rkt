#lang eopl

(require "../states/Customer.rkt")
(require "../blueprints/Account.rkt")
(require "../blueprints/Loan.rkt")
(require "../states/LoanState.rkt")
(require "../commander/commander.rkt")
(provide (all-defined-out))

(define finish
  (lambda (last-month account-types loan-types customers)
    (let ([out (open-output-file "result.txt" #:exists 'replace)])
      (write last-month out)
      (newline out)
      (write-customers account-types loan-types customers out)
      (close-output-port out)
    )
  ) 
)

(define write-customers
  (lambda (account-types customers out)
    (if (null? customers) 
      out
      (cases Customer (car customers)
          (a-customer (id type initial-amount amount deadline-month credit-counter credit interest-rate loans minimum-amount blocked-money creation-time)
            (write "Customer " out)
            (write id out)
            (write "\t" out)
            (write initial-amount out)
            (write "\t" out)            
            (write amount out)
            (write "\t" out)
            (write deadline-month out)
            (write "\t" out)
            (write credit out)
            (write "\t" out)
            (if (has-interest? (get-account-type type account-types))
              (write 0 out)
              (write interest-rate out)
            )
            (write "\t" out)
            (write (time->amount (latest-loan-time loans) loans time->amount) out)
            (write "\t" out)            
            (write (latest-loan-time loans) out)
            (write "\t" out)            
            (write blocked-money out)
            (write "\t" out)            
            ; loan deadline time
            (write (sum-of-debts loans 0) out)
            (newline out)
            (write-customers account-types loan-types (cdr customers) out)
          )
      )
    )
  )
)

(define sum-of-debts
  (lambda (loans debts)
    (if (null? loans)
      debts
      (sum-of-debts (cdr loans) (+ (loan-state->debt (car loans)) debts))
    )
  )
)

(define latest-loan-time
  (lambda (loans)
    (if (null? loans)
      0
      (let ([time (loan-state->time (car loans))])
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

(define time->amount
  (lambda (spec-time loans loan-types)
    (if (null? loans)
      0
      (if (equal? spec-time (loan-state->time (car loans)))
        (loan->amount (get-loan-type (loan-state->type (car loans))) loan-types)
        (time->amount spec-time (cdr loans))
      )
    )
  )
)
