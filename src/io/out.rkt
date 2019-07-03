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
  (lambda (account-types loan-types customers out)
    (if (null? customers) 
      out
      (cases Customer (car customers)
          (a-customer (id type initial-amount amount deadline-month credit-counter credit interest-rate loans minimum-amount blocked-money creation-time)
            (display "Customer " out)
            (write id out)
            (display "\t" out)
            (write initial-amount out)
            (display "\t" out)            
            (write amount out)
            (display "\t" out)
            (write deadline-month out)
            (display "\t" out)
            (write credit out)
            (display "\t" out)
            (if (has-interest? (get-account-type type account-types))
              (write 0 out)
              (write interest-rate out)
            )
            (display "\t" out)
            (write (time->amount (latest-loan-time loans) loans loan-types) out)
            (display "\t" out)            
            (write (latest-loan-time loans) out)
            (display "\t" out)            
            (write blocked-money out)
            (display "\t" out)            
            (write (+ (time->deadline (latest-loan-time loans) loans loan-types) (time->time (latest-loan-time loans) loans loan-types)) out)
            (display "\t" out)            
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
        (loan->amount (get-loan-type (loan-state->type (car loans)) loan-types) )
        (time->amount spec-time (cdr loans))
      )
    )
  )
)

(define time->time
  (lambda (spec-time loans loan-types)
    (if (null? loans)
      0
      (if (equal? spec-time (loan-state->time (car loans)))
        (loan-state->time (car loans))
        (time->time spec-time (cdr loans))
      )
    )
  )
)

(define time->deadline
  (lambda (spec-time loans loan-types)
    (if (null? loans)
      0
      (if (equal? spec-time (loan-state->time (car loans)))
        (loan->return-span (get-loan-type (loan-state->type (car loans)) loan-types) )
        (time->deadline spec-time (cdr loans))
      )
    )
  )
)
