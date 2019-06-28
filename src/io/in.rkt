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
      (let ([command (car raw-commands)])
        (cond
          
          [(regexp-match #rx"Time goes by" command)
            (analyse-commands (cdr raw-commands) (append analysed-commands (list (time))))
          ]
          
          [(regexp-match #px"Customer (\\d+) wants to create an account of type (\\d+). Customer [\\d+] wants to start with (\\d+) Tomans."
            command)
              (let ([match (regexp-match #px"Customer (\\d+) wants to create an account of type (\\d+). Customer [\\d+] wants to start with (\\d+) Tomans." command)])
                (analyse-commands (cdr raw-commands) 
                  (append analysed-commands (list (new-account (string->number (cadr match)) (string->number (caddr match)) (string->number (cadddr match)))))
                )
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
