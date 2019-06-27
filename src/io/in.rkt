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

(define prepare-commands
  (lambda (raw-commands commands)
    (if (null? raw-commands)
      commands
      (cond
        [(regexp-match #rx"Time goes by" 
          (car raw-commands))
          (prepare-commands (cdr raw-commands) (append commands (list (time))))]

        [else commands]
      )
    )
  )
)

(let ([lines (read-file "../samples/sample_input.txt")])
  (define categorized (categorize lines 0 '() '()))
    (let* ([setup (car categorized)]
           [commands (cadr categorized)])
      (prepare-commands commands '())      
    )
)
