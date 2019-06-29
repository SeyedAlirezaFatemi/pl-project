#lang eopl

(provide (all-defined-out))
(require "../io/in.rkt")
(require "../commander/Command.rkt")
(require "../states/Customer.rkt")
(require "../blueprints/Account.rkt")
(require "../blueprints/Loan.rkt")

; (require racket/lazy-require)
; (lazy-require ["../constructor/constructor.rkt" (loan-types)])

(define loan-types (list (a-loan 2 50000000 30000000 24 24 84 625) (a-loan 1 13000000 5000000 48 24 36 875)))

(define account-types (list (an-account 1 #t 10000 200000 #t 72 #t 1 250 #t 20 1 #f #t 500)) )

;(define commands (list (time-command) (time-command) (time-command) (time-command) (transfer-command 3 100000) (time-command) (deposit-command 3 360000) (new-account-command 1 3 1200000) (time-command) (time-command) (new-account-command 1 1 510000) (time-command) (time-command) (time-command) (time-command) (time-command) (new-account-command 2 1 30000) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (deposit-command 1 2600000) (time-command) (time-command) (time-command) (cheque-command 1 200000) (time-command) (time-command) (time-command) (time-command) (time-command) (renewal-command 1) (time-command) (withdraw-command 1 340000) (time-command) (time-command) (time-command) (request-loan-command 1 2) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (deposit-command 1 1300000) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (request-loan-command 1 1) (withdraw-loan-command 1) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (pay-debt-command 1 1000000)) )
(define commands (list (time-command)) )

(define customers '() )

(define month-number 0)

(define do-command
  (lambda (command)
    (cases Command command
        (time-command () 1
        )
        (new-account-command (customer-id account-type initial-balance)
            (let ([new-customer (a-customer customer-id account-type initial-balance initial-balance 10 10 0 10 '() 10 10)])
                (append new-customer customers)
            )
        )
        (deposit-command (customer-id amount) 3
        )
        (renewal-command (customer-id) 4
        )
        (cheque-command (customer-id amount) 5
        )
        (card-command (customer-id amount) 6
        )
        (transfer-command (customer-id amount) 7
        )
        (withdraw-command (customer-id amount) 8
        )
        (close-command (customer-id) 9
        )
        (request-loan-command (customer-id loan-type) 10
        )
        (pay-debt-command (customer-id amount) 11
        )
        (withdraw-loan-command (customer-id) 12
        )
    )
  )
)

(define do-commands
  (lambda (commands)
    (if (null? commands)
      customers
      (
        begin
        (display (do-command (car commands)))
        (newline)
        (do-commands (cdr commands))
      )
    )
  )
)

(define work-on-commands
  (lambda (ls as cs)
    (
      begin
      (set! account-types as)
      (set! loan-types ls)
      (set! commands cs)
      (do-commands commands)
    )
  )
)

; (define )