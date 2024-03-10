#lang racket
; https://stackoverflow.com/questions/8119949/best-way-to-do-input-with-racket
(require "CsvReader.rkt")
; NOTE Since this is such a large list, I opted to use set! for updating the search-results
(define search-results sales-list)

; INPUT FUNCTIONS

; PRE: takes a list from the get-choice function
; POST: Either returns the list if it's size is under 3, or returns the list with only its first 3 elements
(define (reduce-list lst)
  (if (< (length lst) 3)
      lst
      (take lst 3))
  )

; PRE: none
; POST: Gets user input for menu navigation, returns a list of all selections as lowercase letters.
(define (get-choice)
  (display "Select your choices: ")
  (string-split (string-downcase (read-line (current-input-port) 'any)) " ") ; Split string into list, then reducing it.
  )

; PRE: none
; POST: Gets user input for searching, returns input as lower-case.
(define (get-query)
  (display "Enter search query: ")
  (string-downcase (read-line (current-input-port) 'any))
  )

; PRE: none
; POST: Gets user input for a year, returns the year
(define (get-year)
  (define usr-input (string->number (read-line (current-input-port) 'any)))
  (cond
    [(number? usr-input) usr-input]
    [else (begin
          (displayln "Invalid input. Please enter a number.")
          (get-year)
          )]
  )
)

; PRE: none
; POST: Calls get-year for setting the start and end search ranges. Will adjust nonsensical values. Returns a pair of the range
(define (get-range)
  (display "Enter the start year: ")
  (define lower-bound (get-year))
  (display "Enter the end year: ")
  (define upper-bound (get-year))
  ; Manage inputs that don't make sense. 
  (cond
    [(> lower-bound upper-bound) (begin  ; If lower is higher than upper, floor lower to the upper
                                   (define lower-bound upper-bound)
                                   (display "Start year was greater than end year. Setting search range to end year...")
                                   )]
    [(< upper-bound lower-bound) (begin  ; If higher is lower than lower, raise the higher to lower
                                   (define upper-bound lower-bound)
                                   (display "End year was less than start year. Setting search range to start year...")
                                   )]
    )
  (cons lower-bound upper-bound) ; Return a pair of the lower and upper bounds
  )

; DISPLAY FUNCTIONS

(define (display-main-menu)
  (display 
  "What would you like to search with? (3 selections at max)
1: Game Title
2: Date (Year)
3: Publisher
4: Region
5: Genre
")
  )

(define (display-sorting-menu)
  (display 
  "How would you like the data sorted?
1: By Sales
2: By Reviews
")
  )

(define (display-region-menu)
  (display 
  "What region?
1: North America
2: Europe
3: Japan
4: Other
5: Global
")
  )

; FILTERING FUNCTIONS

; PRE: none
; POST: Gets the string to search for from get-query, updates search-results with said query
(define (filter-by-title)
  (display "!!Searching titles!!\n")
  (define search-criteria (get-query))
  (set! search-results
        (filter
         (lambda (sublist)
           (string-contains? (string-downcase (list-ref sublist 1)) search-criteria)) ; JANK solution, but it fixes case sensitivity issues.
         search-results))
  )

; PRE: none
; POST: Gets the year range to search for from get-range, updates search-results with said range
(define (filter-by-date)
  (display "!!Searching by date (year)!!\n")
  (define year-range-pair (get-range))
  (set! search-results
        (filter
         (lambda (sublist)
           (and
            (>= (string->number (list-ref sublist 3)) (car year-range-pair))
            (<= (string->number (list-ref sublist 3)) (cdr year-range-pair))))
         search-results))
  )

; PRE: none
; POST: Gets the string to search for from get-query, updates search-results with said query
(define (filter-by-publisher)
  (display "!!Searching publishers!!\n")
  (define search-criteria (get-query))
  (set! search-results
        (filter
         (lambda (sublist)
           (string-contains? (string-downcase (list-ref sublist 5)) search-criteria))
         search-results))
  )

; PRE: none
; POST: Gets the region a user want to filter from, updates search-results
; NOTE: I wasn't sure what was meant from the project instructions for filtering by region.
; I have opted to make this filter function select games that have greater than 0 sales from a selected region.
(define (region-selection)
  (display-region-menu)
  (define selected-region (get-query))
  (cond
    [(equal? selected-region "1") (filter-by-region 6)] ; NA
    [(equal? selected-region "2") (filter-by-region 7)] ; EU
    [(equal? selected-region "3") (filter-by-region 8)] ; JP
    [(equal? selected-region "4") (filter-by-region 9)] ; OTHER
    [(equal? selected-region "5") (filter-by-region 10)] ; GLOBAL
    [else (begin (display "Not a valid selection\n") (region-selection))]
  )
)

; PRE: an index of the regions column
; POST: Updates search-results with games that have sold more than 0 in the selected region
(define (filter-by-region column-index) ; helper function
  (set! search-results
      (filter
       (lambda (sublist)
         (> (string->number (list-ref sublist column-index)) 0.0)
       search-results)))
  )

; PRE: none
; POST: Gets the string to search for from get-query, updates search-results with said query
; NOTE: Does not do exact matching
(define (filter-by-genre)
  (display "Searching genres\n")
  (define search-criteria (get-query))
  (set! search-results
        (filter
         (lambda (sublist)
           (string-contains? (string-downcase (list-ref sublist 4)) search-criteria))
         search-results))
  )

; PRE: the column index for what to sort by
; POST: Updates search-results to be sorted by the selection
(define (sort-by-selection selection)
  (set! search-results (sort search-results
        (lambda (a b)
          (< (string->number (list-ref a selection)) (string->number (list-ref b selection))))))
  )

; PRE: none
; POST: Prints the sorting menu and asks user for a selection
(define (sorting-selection)
  (display-sorting-menu)
  (define selected-sort (get-query))
  (cond
    ; The search results are already sorted by sales, no purpose for this cond. 
    ;[(equal? selected-sort "1") (sort-by-selection 0)] ; Sales
    [(equal? selected-sort "1")] ; Sales
    [(equal? selected-sort "2") (sort-by-selection 11)] ; Review
    [else (begin (display "Not a valid selection\n") (sorting-selection))]
  )
)

; RUNNING THE MENUS

; PRE: A single string
; POST: calls the relevant functions for filtering.
(define (apply-main-menu-criteria selection)
  (cond
    [(equal? selection "1") (filter-by-title)]
    [(equal? selection "2") (filter-by-date)]
    [(equal? selection "3") (filter-by-publisher)]
    [(equal? selection "4") (region-selection)]
    [(equal? selection "5") (filter-by-genre)]
    [else 0]
  )
  )

; PRE: A list of criteria selections
; POST: Continues sending the criteria one at a time to apply-main-menu-criteria for applying searches
(define (begin-search criteria-to-do)
  (cond
    [(empty? criteria-to-do) ]
    [else (begin
            (apply-main-menu-criteria (first criteria-to-do))
            (begin-search (rest criteria-to-do)))]
  )
)

; PRE: none
; POST: Loops infinitely, continues asking user for inputs to search and sort game sales
(define (main-loop)
  (display-main-menu)
  (define criteria (get-choice))
  (begin-search criteria)
  (sorting-selection)
  (for-each (lambda (sublist) (displayln sublist)) search-results)
  (newline)
  (set! search-results sales-list) ; Reset search results back to normal
  (main-loop) ; Allow user to keep doing new searches
)

(main-loop)
