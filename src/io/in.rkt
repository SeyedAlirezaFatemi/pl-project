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

(write (read-file "../samples/sample_input.txt"))

; (define handle-input
;   (lambda ()
;     (let* ([phase 0]
;            [account-details '()]
;            [loan-details '()]
;            [commands '()])
;       (for ([line (file->lines "../samples/sample_input.txt")])
;         (cond )
;         (if (eq? line "setup") 
;           (set! phase 1)

;         )

;       )
;     )
;   )
; )
