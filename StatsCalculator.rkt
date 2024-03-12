#lang racket/gui
(require racket/string racket/class math/statistics plot)

(define graph-function box-and-whisker)
(define data empty)
(define group-size 5)

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
  (sort mode-list <))

(define (parse-numbers str)
  (map string->number (string-split (string-replace str (regexp "[^0-9 .]") ""))))

(define (histogram data)
  (discrete-histogram (hash-map (foldl
             (lambda (val res)
               (let* ([group (inexact->exact (quotient val group-size))]
                      [val (hash-ref res group 0)])
                 (hash-set res group (+ val 1))))
             (hash)
             data)
            (lambda (key val) (list (format "~a - ~a" (* key group-size) (- (* group-size (+ key 1)) 1)) val)) #t)))

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
                     (set! data (map exact->inexact(sort vals <)))
                     (update-views)))]))

(define reports-panel (new horizontal-panel%
                           [parent frame]))
(define left-panel (new vertical-panel%
                       [parent reports-panel]
                       [alignment '(left top)]
                       [min-width 250]
                       [stretchable-width #f]
                       [style '(border)]))

(define stats-panel (new vertical-panel%
                       [parent left-panel]
                       [min-height 250]
                       [stretchable-height #f]
                       [alignment '(left top)]
                       [style '(border)]))

(define text-count (new message%
                 [label (string-append "Count: ")]
                 [parent stats-panel]
                 [auto-resize #t]))
(define text-mean (new message%
                 [label (string-append "Mean: ")]
                 [parent stats-panel]
                 [auto-resize #t]))
(define text-median (new message%
                 [label (string-append "Median: ")]
                 [parent stats-panel]
                 [auto-resize #t])) 
(define text-mode (new message%
                 [label (string-append "Mode: ")]
                 [parent stats-panel]
                 [auto-resize #t]))
(define text-std-dev (new message%
                 [label (string-append "Std Deviation: ")]
                 [parent stats-panel]
                 [auto-resize #t]))

(define controls-panel (new vertical-panel%
                       [parent left-panel]
                       [alignment '(left top)]))

(define graph-select (new choice%
                          [label "Graph type:"]
                          [choices '("box and whiskers" "histogram")]
                          [parent controls-panel]
                          [callback (lambda (choice event)
                                      (cond
                                        [(= (send choice get-selection) 0)
                                         (set! graph-function box-and-whisker)
                                         (send graph-controls change-children (lambda (a-list) (list box-and-whisker-controls)))
                                         (draw-graph data graph-function)]
                                        [(= (send choice get-selection) 1)
                                         (set! graph-function histogram)
                                         (send graph-controls change-children (lambda (a-list) (list histogram-controls)))
                                         (draw-graph data graph-function)]))]))

(define graph-controls (new vertical-panel%
                            [parent controls-panel]))
(define histogram-controls (new vertical-panel%
                                [parent graph-controls]))
(define histogram-group-size-input
  (new text-field%
       [label "Set group size"]
       [parent histogram-controls]
       [init-value (number->string group-size)]
       [callback (lambda (text-field event)
                   (let* ([input (send text-field get-value)]
                          [val (cond [(empty? (parse-numbers input)) 1]
                                     [(> 1 (first (parse-numbers input))) 1]
                                     [else (first (parse-numbers input))])])
                     (set! group-size val)
                     (update-views)))]))

(define box-and-whisker-controls (new vertical-panel%
                                      [parent graph-controls]))

(define graph-pane (new vertical-pane%
                        [parent reports-panel]))

(define canvas (new editor-canvas%
                    [parent graph-pane]
                    [style '(no-focus)]))

(define editor (new text%))

(send canvas set-editor editor)

(define (update-views)
  (cond [(> (length data) 0) 
         (send text-count set-label (string-append "Count: " (number->string (length data))))
         (send text-mean set-label (string-append "Mean: " (number->string (mean data))))
         (send text-median set-label (string-append "Median: " (number->string (median data))))
         (send text-mode set-label (string-append "Mode: " (string-join (map ~a (mode data)))))
         (send text-std-dev set-label (string-append "Std Deviation: " (number->string (stddev data))))
         (draw-graph data graph-function)]))

(define (draw-graph data graph-function)
  (cond [(not (empty? data))
       (send editor delete 'start)
       (send editor insert (plot-snip (graph-function data)))]))

(send graph-controls change-children (lambda (a-list) (list box-and-whisker-controls)))

(send frame show #t)