#lang racket

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

(let ([lines (read-file "../samples/sample_input.txt")])
  (write (categorize lines 0 '() '()))
)
