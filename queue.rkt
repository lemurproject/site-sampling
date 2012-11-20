#lang racket

(define queue-tag 'queue)
(define empty-queue (cons queue-tag '()))

(define (make-queue) empty-queue)

(define (queue? obj)
  (and (list? obj) (eq? (first obj) queue-tag)))

(define (enqueue queue item)
  (cons queue-tag (cons item (rest queue))))
      
