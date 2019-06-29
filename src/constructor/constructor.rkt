#lang racket

(require "../io/in.rkt")
(require "../commander/Command.rkt")
(require "../blueprints/Account.rkt")

(provide analyse-input-file)

; phase = 0 => setup
; phase = 1 => commands
; Returns a list with two elements: (setup commands)
(define categorize 
  (lambda (lines phase setups commands)
    (if (null? lines)
      (list setups commands)
      (let ([line (car lines)])  
        (cond 
          [(equal? line "commands") (categorize (cdr lines) 1 setups commands)]
          [(equal? line "setup") (categorize (cdr lines) 0 setups commands)]
          [else
            (if (eq? phase 0)
              (categorize (cdr lines) phase (append setups (list line)) commands)
              (categorize (cdr lines) phase setups (append commands (list line)))
            )
          ]
        )
      )
    )
  )
)

(define analyse-commands
  (lambda (raw-commands analysed-commands)
    (if (null? raw-commands)
      analysed-commands
      (let* ([raw-command (car raw-commands)] [rest-raw-commands (cdr raw-commands)])
        (cond
          ; time-command
          [(regexp-match #rx"^Time goes by" raw-command)
            (analyse-commands rest-raw-commands (append analysed-commands (list (time-command))))
          ]
          ; new-account-command
          [(regexp-match #px"^Customer (\\d+) wants to create an account of type (\\d+). Customer [\\d+] wants to start with (\\d+) Tomans." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands
                (append analysed-commands (list (new-account-command (string->number (cadr match)) (string->number (caddr match)) (string->number (cadddr match)))))
              )
            )
          ]
          ; deposit-command
          [(regexp-match #px"^Customer (\\d+) adds (\\d+) Tomans to his account." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (deposit-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]
          ; renewal-command
          [(regexp-match #px"^Customer (\\d+) requests renewal." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (renewal-command (string->number (cadr match))))))
            )
          ]
          ; cheque-command
          [(regexp-match #px"^Customer (\\d+) writes a cheque for (\\d+) Tomans." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (cheque-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]
          ; card-command
          [(regexp-match #px"^Customer (\\d+) spends (\\d+) Tomans via his card." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (card-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]
          ; transfer-command
          [(regexp-match #px"^Customer (\\d+) transfers (\\d+) Tomans." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (transfer-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]
          ; withdraw-command
          [(regexp-match #px"^Customer (\\d+) withdraws (\\d+) Tomans from his account." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (withdraw-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]
          ; close-command
          [(regexp-match #px"^Customer (\\d+) closes his account." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (close-command (string->number (cadr match))))))
            )
          ]
          ; request-loan-command
          [(regexp-match #px"^Customer (\\d+) requests a loan of type (\\d+)." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (request-loan-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]
          ; pay-debt-command
          [(regexp-match #px"^Customer (\\d+) pays (\\d+) Tomans of his debt." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (pay-debt-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]
          ; withdraw-loan-command
          [(regexp-match #px"^Customer (\\d+) withdraws the loan." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (withdraw-loan-command (string->number (cadr match))))))
            )
          ]
          ; error
          [else (analyse-commands (cdr raw-commands) analysed-commands)]
        )
      )
    )
  )
)

; phase = 0 => account
; phase = 1 => loan 
(define analyse-setups
  (lambda (setups phase loan-types account-types current)
    (write current)
    (if (null? setups)
      (list loan-types account-types)
      (let* ([setup (car setups)] [rest-setups (cdr setups)])
        (cond
          ; account-type-setup
          [(regexp-match #px"^Account type (\\d+)" setup) =>
            (lambda (match)
              (analyse-setups rest-setups 0 loan-types account-types (append current (list (string->number (cadr match)))))
            )
          ]
          ; account-current-account-setup
          [(regexp-match #px"^current-account\\? (true|false)" setup) =>
            (lambda (match)
              (if (equal? (cadr match) "true")
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #t)))
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #f)))
              )
            )
          ]
          ; account-bank-fee-setup
          [(regexp-match #px"^bank-fee\\? (\\d+) Tomans" setup) =>
            (lambda (match)
              (analyse-setups rest-setups 0 loan-types account-types (append current (list (string->number (cadr match)))))
            )
          ]
          ; account-minimum-deposit-setup
          [(regexp-match #px"^minimum-deposit\\? (\\d+) Tomans" setup) =>
            (lambda (match)
              (analyse-setups rest-setups 0 loan-types account-types (append current (list (string->number (cadr match)))))
            )
          ]
          ; account-monthly-setup
          [(regexp-match #px"^monthly\\? (true|false)" setup) =>
            (lambda (match)
              (if (equal? (cadr match) "true")
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #t)))
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #f)))
              )
            )
          ]
          ; account-period-setup
          [(regexp-match #px"^period\\? (\\d+) months" setup) =>
            (lambda (match)
              (analyse-setups rest-setups 0 loan-types account-types (append current (list (string->number (cadr match)))))
            )
          ]
          ; account-renewable-setup
          [(regexp-match #px"^renewable\\? (true|false)" setup) =>
            (lambda (match)
              (if (equal? (cadr match) "true")
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #t)))
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #f)))
              )
            )
          ]
          ; account-interest-rate-setup
          [(regexp-match #px"^interest-rate\\? (\\d+) percent" setup) =>
            (lambda (match)
              (analyse-setups rest-setups 0 loan-types account-types (append current (list (string->number (cadr match)))))
            )
          ]
          ; account-credit-setup
          [(regexp-match #px"^credit\\? (\\d+) units" setup) =>
            (lambda (match)
              (analyse-setups rest-setups 0 loan-types account-types (append current (list (string->number (cadr match)))))
            )
          ]
          ; account-variability-setup
          [(regexp-match #px"^variability\\? (true|false)" setup) =>
            (lambda (match)
              (if (equal? (cadr match) "true")
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #t)))
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #f)))
              )
            )
          ]
          ; account-span-for-increase-setup
          [(regexp-match #px"^span-for-increase\\? (\\d+) months" setup) =>
            (lambda (match)
              (analyse-setups rest-setups 0 loan-types account-types (append current (list (string->number (cadr match)))))
            )
          ]
          ; account-increase-rate-setup
          [(regexp-match #px"^increase-rate\\? ([0-9]*\\.?[0-9]*) percent" setup) =>
            (lambda (match)
              (analyse-setups rest-setups 0 loan-types account-types (append current (list (string->number (cadr match)))))
            )
          ]
          ; account-has-cheque-setup
          [(regexp-match #px"^has-cheque\\? (true|false)" setup) =>
            (lambda (match)
              (if (equal? (cadr match) "true")
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #t)))
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #f)))
              )
            )
          ]
          ; account-has-card-setup
          [(regexp-match #px"^has-card\\? (true|false)" setup) =>
            (lambda (match)
              (if (equal? (cadr match) "true")
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #t)))
                (analyse-setups rest-setups 0 loan-types account-types (append current (list #f)))
              )
            )
          ]
          ; account-transfer-fee-setup
          [(regexp-match #px"^transfer-fee\\? (\\d+) Tomans" setup) =>
            (lambda (match)
              (let* ([id (car current)]
                     [has-interest (not (cadr current))]
                     [fee (caddr current)]
                     [minimum-deposit (cadddr current)]
                     [monthly (car (cddddr current))]
                     [period (cadr (cddddr current))]
                     [renewable (caddr (cddddr current))]
                     [interest-rate (cadddr (cddddr current))]
                     [credit (car (cddddr (cddddr current)))]
                     [has-variable-interest (cadr (cddddr (cddddr current)))]
                     [span-for-increase (caddr (cddddr (cddddr current)))]
                     [increase-rate (cadddr (cddddr (cddddr current)))]
                     [has-cheque (car (cddddr (cddddr (cddddr current))))]
                     [has-card (cadr (cddddr (cddddr (cddddr current))))]
                     [transfer-fee (string->number (cadr match))]
                     )
                (let ([new-account (an-account id has-interest fee minimum-deposit monthly period renewable increase-rate credit has-variable-interest span-for-increase increase-rate has-cheque has-card transfer-fee)])
                  (analyse-setups rest-setups 0 loan-types (cons new-account account-types) '())
                ) 
              )
            )
          ]
          [else (analyse-setups rest-setups phase loan-types account-types current)]
        )
      )
    )
  )
)

(define analyse-input-file
  (lambda (file-path)
    (let ([lines (read-file file-path)])
      (let ([categorized (categorize lines 0 '() '())])
        (let* ([setups (car categorized)]
              [raw-commands (cadr categorized)])
          (let* ([commands (analyse-commands raw-commands '())]
                [configurations (analyse-setups setups 0 '() '() '())])
            (let* ([loan-types (car configurations)]
                   [account-types (cadr configurations)])
              (list loan-types account-types commands)
            )
          )   
        )
      )
    )
  )
)
