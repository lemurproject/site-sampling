#lang racket

(require "common.rkt")
(require srfi/1)

(define (get-stats html-xexpr)
  (list (css-classes html-xexpr) (css-histogram html-xexpr)))
(provide get-stats)

;; fetch all css classes on a given page
(define (css-classes html-xexpr)
  (remove-duplicates (fetch-classes html-xexpr)))
  
(define (css-histogram html-xexpr)
  (let ((classes-on-page (css-classes html-xexpr)))
    (zip
     classes-on-page
     (map
      (lambda (x)
	(count (lambda (y) (equal? x y)) classes-on-page))
      classes-on-page))))

(define (jaccard-similarity doc1-stat doc2-stat)
  (let ((intersection (filter
		       (lambda (x) (member x doc2-stat))
		       doc1-stat))
	(union (append
		doc2-stat
		(filter
		 (lambda (x) (not (member x doc2-stat)))
		 doc1-stat))))
    (/ (length intersection) (length union))))
