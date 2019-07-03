#lang racket

(provide (all-defined-out))
(require eopl)
(require "../io/in.rkt")
(require "../commander/Command.rkt")
(require "../states/Customer.rkt")
(require "../blueprints/Account.rkt")
(require "../blueprints/Loan.rkt")

(define loan-types '())
(define account-types '())
(define commands '())
(define customers '())
(define to-do '())
(define month-number 0)

;(define exn-test
;  (lambda ()
;    (with-handlers ([exn:fail? (lambda (exn) 'discarded)])
;        (begin
;        (display 1)
;        (newline)
;        (error 'chi!)
;        (display 2)
;        (newline))
;    )
;   )
;)

; (an-account  (id has-interest fee minimum-deposit monthly
;                period renewable interest-rate credit has-variable-interest
;                span-for-increase increase-rate has-cheque has-card transfer-fee ))

(define get-account-type
  (lambda (search-id account-type-list)
    (if (null? account-type-list)
      (begin (raise 'account-not-found))
      (let ([acc (car account-type-list)])
        (cases Account acc
          (an-account  (id has-interest fee minimum-deposit monthly 
                        period renewable interest-rate credit has-variable-interest 
                        span-for-increase increase-rate has-cheque has-card transfer-fee)
            (if (= id search-id)
              acc
              (get-account-type search-id (cdr account-type-list))
            )
          )
        )
      )
    )
  )
)

(define get-loan-type
  (lambda (search-id loan-type-list)
    (if (null? loan-type-list)
      (raise 'loan-not-found)
      (let ([loan (car loan-type-list)])
        (let ([loan-id (loan->id loan)])
          (if (= loan-id search-id)
            loan
            (get-loan-type search-id (cdr loan-type-list))
          )
        )
      )
    )
  )
)

(define get-customer
  (lambda (search-id customer-list)
    (if (null? customer-list)
      (begin (raise 'customer-not-found))
      (let ([customer (car customer-list)])
        (let ([customer-id (customer->id customer)])
          (if (= customer-id search-id)
            customer
            (get-customer search-id (cdr customer-list))
          )
        )
      )
    )
  )
)

(define get-customers-account
  (lambda (customer)
    (get-account-type (customer->type customer) account-types)
  )
)

(define modify-customer
  (lambda (customer customer-list)
    (if (null? customer-list)
      (raise 'customer-not-found-for-save)
      (cases Customer customer
        (a-customer (id type initial-amount amount
                      deadline-month credit-counter credit
                      interest-rate loans minimum-amount blocked-money)
          (let ([head (car customer-list)])
            (cases Customer head
              (a-customer (head-id head-type head-initial-amount head-amount
                            head-deadline-month head-credit-counter head-credit
                            head-interest-rate head-loans head-minimum-amount head-blocked-money)
                (if (= head-id id)
                  (cons customer (cdr customer-list))
                  (cons head (modify-customer customer (cdr customer-list)))
                )
              )
            )
          )
        )
      )
    )
  )
)

(define delete-customer
  (lambda (search-id customer-list)
    (if (null? customer-list)
      (raise 'customer-not-found-for-deletion)
      (let ([head (car customer-list)])
        (cases Customer head
          (a-customer (head-id head-type head-initial-amount head-amount
                       head-deadline-month head-credit-counter head-credit
                       head-interest-rate head-loans head-minimum-amount head-blocked-money)
            (if (= head-id search-id)
              (cdr customer-list)
              (cons head (delete-customer search-id (cdr customer-list)))
            )
          )
        )
      )
    )
  )
)

(define save-customer
  (lambda (customer)
    (begin
      (set! customers (modify-customer customer customers))
    )
  )
)

(define punish
  (lambda (customer account)
    (cases Customer customer
      (a-customer (customer-id type initial-amount amount
                  deadline-month credit-counter credit
                  interest-rate loans minimum-amount blocked-money)
        (let ([account (get-customers-account customer)])
          (cases Account account
            (an-account  (id has-interest fee minimum-deposit monthly
                          period renewable interest-rate account-credit has-variable-interest
                          span-for-increase increase-rate has-cheque has-card transfer-fee)
              (let ([modified-customer
                  (a-customer id type initial-amount
                      amount
                      deadline-month
                      0
                      (- credit (/ account-credit 2))
                      interest-rate loans minimum-amount blocked-money
                  )])
                (begin
                  (save-customer modified-customer)                   ; LOG
                  (display "Punishing customer #")                    ; LOG
                  (display customer-id)                               ; LOG
                  (newline)                                           ; LOG
                )
              )
            )
          )
        )
      )
    )
  )
)

(define do-command
  (lambda (command)
    (with-handlers ([symbol? (lambda (exn) (begin (display "Exception: ") (display exn) (newline)))])     ; LOG
      (cases Command command
        (time-command ()
          (set! month-number (+ month-number 1))
        )
        (new-account-command (customer-id account-type initial-balance)
          (let ([acc (get-account-type account-type account-types) ])
            (cases Account acc 
              (an-account (id has-interest
                           fee minimum-deposit monthly period
                           renewable interest-rate credit
                           has-variable-interest span-for-increase
                           increase-rate has-cheque has-card transfer-fee)
                (let ([new-customer
                      (a-customer customer-id account-type
                      (- initial-balance fee)         ; => initial-amount
                      (- initial-balance fee)         ; => amount
                      (+ month-number period)         ; => deadline-month
                      0                               ; => credit-counter
                      0                               ; => credit
                      interest-rate
                      '()                             ; => loans
                      minimum-deposit
                      0                               ; => blocked-money
                      )])
                  (begin
                    (set! customers (cons new-customer customers))
                    (display 'account-created!) ; LOG
                    (newline)                   ; LOG
                  )
                )
              )
            )
          )
        )
        (deposit-command (customer-id add-amount)
          (let ([customer (get-customer customer-id customers)])
            (cases Customer customer
              (a-customer (id type initial-amount amount
                            deadline-month credit-counter credit
                            interest-rate loans minimum-amount blocked-money)
                (let ([modified-customer
                    (a-customer id type initial-amount
                        (+ amount add-amount)
                        deadline-month credit-counter credit
                        interest-rate loans minimum-amount blocked-money
                    )])
                  (begin
                    (save-customer modified-customer)
                    (display add-amount)                                ; LOG
                    (display "$ added to the account of customer #")    ; LOG
                    (display customer-id)                               ; LOG
                    (newline)                                           ; LOG
                  )
                )
              )
            )
          )
        )
        (renewal-command (customer-id)
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (if (>= deadline-month month-number)
                            (raise 'wait-for-the-deadline)
                            (let ([account (get-customers-account customer)])
                               (cases Account account
                                 (an-account  (id has-interest fee minimum-deposit monthly
                                                period renewable interest-rate credit has-variable-interest
                                                span-for-increase increase-rate has-cheque has-card transfer-fee)
                                    (if renewable
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                amount
                                                (+ month-number period)
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                              ; LOG
                                            (display "We have the renewal of customer #")       ; LOG
                                            (display customer-id)                               ; LOG
                                            (newline)                                           ; LOG
                                          )
                                        )
                                        (raise 'not-renewable)
                                    )
                                 )
                               )
                            )
                        )
                    )
                )
            )
        )
        (cheque-command (customer-id cheque-amount)
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (let ([account (get-customers-account customer)])
                           (cases Account account
                             (an-account  (id has-interest fee minimum-deposit monthly
                                            period renewable interest-rate credit has-variable-interest
                                            span-for-increase increase-rate has-cheque has-card transfer-fee)
                                (if has-cheque
                                    (if (>= (- amount cheque-amount) minimum-deposit)
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                (- amount cheque-amount)
                                                deadline-month
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                   ; LOG
                                            (display cheque-amount)                             ; LOG
                                            (display "$ is paid by cheque by customer #")       ; LOG
                                            (display customer-id)                               ; LOG
                                            (newline)                                           ; LOG
                                          )
                                        )
                                        (raise 'not-enough-money-for-cheque)
                                    )
                                    (begin
                                        (punish customer account)
                                        (raise 'not-chequeable!)
                                    )
                                )
                             )
                           )
                        )
                    )
                )
            )
        )
        (card-command (customer-id card-amount)
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (let ([account (get-customers-account customer)])
                           (cases Account account
                             (an-account  (id has-interest fee minimum-deposit monthly
                                            period renewable interest-rate credit has-variable-interest
                                            span-for-increase increase-rate has-cheque has-card transfer-fee)
                                (if has-card
                                    (if (>= (- amount card-amount) minimum-deposit)
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                (- amount card-amount)
                                                deadline-month
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                   ; LOG
                                            (display card-amount)                               ; LOG
                                            (display "$ is paid by card by customer #")         ; LOG
                                            (display customer-id)                               ; LOG
                                            (newline)                                           ; LOG
                                          )
                                        )
                                        (raise 'not-enough-money-for-card)
                                    )
                                    (begin
                                        (punish customer account)
                                        (raise 'not-cardable!)
                                    )
                                )
                             )
                           )
                        )
                    )
                )
            )
        )
        (transfer-command (customer-id transfer-amount) 7
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (let ([account (get-customers-account customer)])
                           (cases Account account
                             (an-account  (id has-interest fee minimum-deposit monthly
                                            period renewable interest-rate credit has-variable-interest
                                            span-for-increase increase-rate has-cheque has-card transfer-fee)
                                (if has-card
                                    (if (>= (- amount transfer-amount) minimum-deposit)
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                (- amount transfer-amount)
                                                deadline-month
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                   ; LOG
                                            (display transfer-amount)                               ; LOG
                                            (display "$ is paid by transfer command by customer #")         ; LOG
                                            (display customer-id)                               ; LOG
                                            (newline)                                           ; LOG
                                          )
                                        )
                                        (raise 'not-enough-money-for-transfer)
                                    )
                                    (begin
                                        (punish customer account)
                                        (raise 'not-transferable!)
                                    )
                                )
                             )
                           )
                        )
                    )
                )
            )
        )
        (withdraw-command (customer-id withdraw-amount)
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (let ([account (get-customers-account customer)])
                           (cases Account account
                             (an-account  (id has-interest fee minimum-deposit monthly
                                            period renewable interest-rate credit has-variable-interest
                                            span-for-increase increase-rate has-cheque has-card transfer-fee)
                                (if has-card
                                    (if (>= (- amount withdraw-amount) minimum-deposit)
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                (- amount withdraw-amount)
                                                deadline-month
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                   ; LOG
                                            (display withdraw-amount)                               ; LOG
                                            (display "$ is paid by withdraw command by customer #")         ; LOG
                                            (display customer-id)                               ; LOG
                                            (newline)                                           ; LOG
                                          )
                                        )
                                        (raise 'not-enough-money-for-withdraw)
                                    )
                                    (begin
                                        (punish customer account)
                                        (raise 'not-withdrawable!)
                                    )
                                )
                             )
                           )
                        )
                    )
                )
            )
        )
        (close-command (customer-id)
            (set! customers (delete-customer customer-id customers))
        )
        (request-loan-command (customer-id loan-type) 
          (let* ([customer (get-customer customer-id customers)]
                 [loan (get-loan-type loan-type)])
            10
          )
        )
        (pay-debt-command (customer-id amount) 11
        )
        (withdraw-loan-command (customer-id) 12
        )
      )
    )
  )
)

(define do-commands
  (lambda (commands)
    (if (null? commands)
      customers
      (begin
        (do-command (car commands))
        (display month-number)                   ; LOG
        (newline)                                ; LOG
        (display customers)                      ; LOG
        (newline)                                ; LOG
        (do-commands (cdr commands))
      )
    )
  )
)

(define work-on-commands
  (lambda (ls as cs)
    (begin
      (display 'Started!!)                      ; LOG
      (newline)                                 ; LOG
      (set! account-types as)
      (set! loan-types ls)
      (set! commands cs)
      (do-commands commands)
    )
  )
)
