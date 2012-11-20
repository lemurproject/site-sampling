#lang racket

; Randomly Sample Websites and build a sitemap

(require net/url)
(require srfi/1)
(require (planet neil/html-parsing:2:0))

(require "common.rkt")
(require "document-stats.rkt")


(define *QUEUE* '())
(define *DOCUMENT-STATS* '())
(define *TO-BE-EXPLORED* 1000)


(define (sample-site seed-url)
  (set! *QUEUE* (cons seed-url *QUEUE*))
  (sample (random-dequeue)))

(define (sample url)
  (let* ((html-xexpr (get-html-xexpr url)))
    (when (not (= *TO-BE-EXPLORED* 0))
      (display *TO-BE-EXPLORED*)
      (display (newline))
      (enqueue-links (string->url url) (fetch-links html-xexpr))
      (set! *DOCUMENT-STATS* (cons (list url (get-stats html-xexpr)) *DOCUMENT-STATS*))
      (set! *TO-BE-EXPLORED* (- *TO-BE-EXPLORED* 1))
      (sample (random-dequeue)))))

(define (enqueue-links base links)
  (set! *QUEUE* (append
		 (map
		  url->string
		  (filter
		   (lambda (x) (equal? (url-host base) (url-host x)))
		   (map 
		    (lambda (x) (combine-url/relative base x)) 
		    links)))
		  *QUEUE*)))

(define (random-dequeue)
  (let ((choice (random 2))
	(returning '*))
    (if (= choice 0)
	(begin
	  (set! returning (first *QUEUE*))
	  (set! *QUEUE* (rest *QUEUE*))
	  returning)
	(begin
	  (set! returning (last *QUEUE*))
	  (set! *QUEUE* (drop-right *QUEUE* 1))
	  returning))))
