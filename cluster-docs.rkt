#lang racket
;; clustering based on css classes on a page

(require "common.rkt")

(define (distance-matrix docs-classes-list)
  (map
   (lambda (doc1-class)
     (map (lambda (doc2-class) (jaccard-similarity (second doc1-class) (second doc2-class)))
	  docs-classes-list))
     docs-classes-list))

(define (jaccard-similarity doc1-stat doc2-stat)
  (if (and (null? doc1-stat) (null? doc2-stat))
      1
      (let ((intersection (filter
			   (lambda (x) (member x doc2-stat))
			   doc1-stat))
	    (union (append
		    doc2-stat
		    (filter
		     (lambda (x) (not (member x doc2-stat)))
		     doc1-stat))))
	(/ (length intersection) (length union)))))
