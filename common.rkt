#lang racket

(require (planet neil/html-parsing:2:0))
(require net/url)
(require (planet clements/sxml2:1:3))


(define sxpath-get-classes-query "//@class/text()")
(define sxpath-get-links-query "//a/@href/text()")


(define (get-html-xexpr url-string)
  (html->xexp (port->string (get-pure-port (string->url url-string)))))
(provide get-html-xexpr)

(define (fetch-links html-xexpr)
  ((sxpath "//a/@href/text()") html-xexpr))
(provide fetch-links)

(define (fetch-classes html-xexpr)
  ((sxpath sxpath-get-classes-query) html-xexpr))
(provide fetch-classes)
