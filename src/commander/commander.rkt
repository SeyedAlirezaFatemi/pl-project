#lang racket

(provide (all-defined-out))
(require "../io/in.rkt")
(require "../commander/Command.rkt")
(require "../states/Customer.rkt")
(require "../blueprints/Account.rkt")
(require "../blueprints/Loan.rkt")
(require eopl)

(define loan-types (list (a-loan 2 50000000 30000000 24 24 84 625) (a-loan 1 13000000 5000000 48 24 36 875)))

(define account-types (list (an-account 1 #t 10000 200000 #t 72 #t 1 250 #t 20 1 #f #t 500)) )

;(define commands (list (time-command) (time-command) (time-command) (time-command) (transfer-command 3 100000) (time-command) (deposit-command 3 360000) (new-account-command 1 3 1200000) (time-command) (time-command) (new-account-command 1 1 510000) (time-command) (time-command) (time-command) (time-command) (time-command) (new-account-command 2 1 30000) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (deposit-command 1 2600000) (time-command) (time-command) (time-command) (cheque-command 1 200000) (time-command) (time-command) (time-command) (time-command) (time-command) (renewal-command 1) (time-command) (withdraw-command 1 340000) (time-command) (time-command) (time-command) (request-loan-command 1 2) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (deposit-command 1 1300000) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (request-loan-command 1 1) (withdraw-loan-command 1) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (time-command) (pay-debt-command 1 1000000)) )
(define commands (list (time-command)) )


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


(define customers '() )

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
                span-for-increase increase-rate has-cheque has-card transfer-fee )
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

(define get-customer
  (lambda (search-id customer-list)
      (if (null? customer-list)
        (begin (raise 'customer-not-found) )
        (let ([customer (car customer-list)])
          (cases Customer customer
            (a-customer (id type initial-amount amount
                        deadline-month credit-counter credit
                        interest-rate loans minimum-amount blocked-money)
              (if (= id search-id)
                customer
                (get-customer search-id (cdr customer-list))
              )
            )
          )
        )
      )
  )
)


(define get-customers-account
  (lambda (customer)
      (cases Customer customer
        (a-customer (id type initial-amount amount
                    deadline-month credit-counter credit
                    interest-rate loans minimum-amount blocked-money)
            (get-account-type type account-types)
        )
      )
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
                        (save-customer modified-customer)                              ; LOG
                        (display "Punishing customer #")       ; LOG
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

(define month-number 0)

(define do-command
  (lambda (command)
    (with-handlers ([symbol? (lambda (exn) (begin(display exn) (newline)))])            ; LOG
      (cases Command command
        (time-command ()
          (set! month-number (+ month-number 1))
        )
        (new-account-command (customer-id account-type initial-balance)
          (let ([acc (get-account-type account-type account-types) ])
            (cases Account acc (an-account (id has-interest
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
        (transfer-command (customer-id amount) 7
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
)

(define do-commands
  (lambda (commands)
    (if (null? commands)
      customers
      (begin
        (do-command (car commands))
        (display month-number)                   ; LOG
        (display " ")                            ; LOG
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