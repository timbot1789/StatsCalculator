#lang racket/gui
(require racket/string racket/class)

(define (reduce func list)
  (if (null? list)
      (func)
  (if (null? (cdr list))
      (car list)
      (func (car list) (reduce func (cdr list))))))

(define (mean data)
      (if (> (length data) 0)
          (/ (reduce + data) (length data))
          0))

(define (median data)
  (let ([len (length data)])
    (if (> len 0)
        (if
         (= (modulo len 2) 1)
         (list-ref data (floor (/ len 2)))
         (/ (+
             (list-ref data (- (/ len 2) 1))
             (list-ref data (/ len 2))) 2))
        0)))

(define (mode data)
  (define hash (make-weak-hash))
  (for-each (lambda (arg)
              (let ([val (hash-ref! hash arg 0)])
                (hash-set! hash arg (+ val 1))))
            data)
  (define max-count 2)
  (define mode-list '())
  (hash-map hash (lambda (key value)
                   (cond
                     [(> value max-count)
                      (set! max-count value)
                      (set! mode-list (list key))]
                     [(= value max-count)
                      (set! mode-list (cons key mode-list))])))
  mode-list)

(define (parse-numbers str)
  (map string->number (string-split (string-replace str (regexp "[^0-9 .]") ""))))

(define frame (new frame%
                   [label "Stats Calculator"]
                   [width 750]
                   [height 750]))

(define number-entry
  (new text-field%
       [label "data set"]
       [parent frame]
       [callback (lambda (text-field event)
                   (let* ([input (send text-field get-value)]
                          [vals (parse-numbers input)])
                     (update-views (map exact->inexact(sort vals <)))))]))


(define text-count (new message%
                 [label (string-append "Count: ")]
                 [parent frame]
                 [min-width 250]))
(define text-mean (new message%
                 [label (string-append "Mean: ")]
                 [parent frame]
                 [min-width 250]))
(define text-median (new message%
                 [label (string-append "Median: ")]
                 [parent frame]
                 [min-width 250])) 
(define text-mode (new message%
                 [label (string-append "Mode: ")]
                 [parent frame]
                 [min-width 250]))

(define (update-views data)
  (send text-count set-label (string-append "Count: " (number->string (length data))))
  (send text-mean set-label (string-append "Mean: " (number->string (mean data))))
  (send text-median set-label (string-append "Median: " (number->string (median data))))
  (send text-mode set-label (string-append "Mode: " (string-join (map ~a (mode data))))))

(send frame show #t)