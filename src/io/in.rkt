#lang racket

(require "../commander/Command.rkt")

(define read-file
  (lambda (file-location)
    (let ([lines '()])
      (for ([line (file->lines file-location)])
        (if (not (or (equal? line "\n") (equal? line "")))
          (set! lines (append lines (list line)))
          null
        )
      )
      lines
    )
  )
)

; phase = 0 => setup
; phase = 1 => commands
; Returns a list with two elements: (setup commands)
(define categorize 
  (lambda (lines phase setup commands)
    (if (null? lines)
      (list setup commands)
      (let ([line (car lines)])  
        (cond 
          [(equal? line "commands") (categorize (cdr lines) 1 setup commands)]
          [(equal? line "setup") (categorize (cdr lines) 0 setup commands)]
          [else
            (if (eq? phase 0)
              (categorize (cdr lines) phase (append setup (list line)) commands)
              (categorize (cdr lines) phase setup (append commands (list line)))
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
          [(regexp-match #rx"Time goes by" raw-command)
            (analyse-commands rest-raw-commands (append analysed-commands (list (time-command))))
          ]
          ; new-account-command
          [(regexp-match #px"Customer (\\d+) wants to create an account of type (\\d+). Customer [\\d+] wants to start with (\\d+) Tomans." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands
                (append analysed-commands (list (new-account-command (string->number (cadr match)) (string->number (caddr match)) (string->number (cadddr match)))))
              )
            )
          ]
          ; deposit-command
          [(regexp-match #px"Customer (\\d+) adds (\\d+) Tomans to his account." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (deposit-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]
          ; renewal-command
          [(regexp-match #px"Customer (\\d+) requests renewal." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (renewal-command (string->number (cadr match))))))
            )
          ]
          ; cheque-command
          [(regexp-match #px"Customer (\\d+) writes a cheque for (\\d+) Tomans." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (cheque-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]
          ; card-command
          [(regexp-match #px"Customer (\\d+) spends (\\d+) Tomans via his card." raw-command) =>
            (lambda (match)
              (analyse-commands rest-raw-commands (append analysed-commands (list (card-command (string->number (cadr match)) (string->number (caddr match))))))
            )
          ]

          [else (analyse-commands (cdr raw-commands) analysed-commands)]
        )
      )
    )
  )
)

(let ([lines (read-file "../samples/sample_input.txt")])
  (define categorized (categorize lines 0 '() '()))
    (let* ([setup (car categorized)]
           [raw-commands (cadr categorized)])
      (analyse-commands raw-commands '())      
    )
)
