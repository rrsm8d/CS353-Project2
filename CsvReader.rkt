#lang racket
; https://stackoverflow.com/questions/10883426/how-to-download-and-parse-a-csv-file-in-racket
(require csv-reading)
; File → Install Package -> csv-reading



; PRE: A list of rules for reading csv files. All possible rules found here: https://docs.racket-lang.org/csv-reading/index.html
; POST: a set of rules for use with csv->list
(define make-sales-csv-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t)))
  )

; PRE: None
; POST: Returns a list of strings, containing the columns of a row. Column names and indices removed
(define sales-list
  (map cdr ; To remove indices
   (rest ; To remove the column names (the first list)
    (csv->list (make-sales-csv-reader (open-input-file "Video Games Sales.csv"))))
   )
  )

(provide sales-list) 