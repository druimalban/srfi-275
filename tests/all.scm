;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(import (srfi :14 char-sets)
        (srfi :64 testing)
        (srfi :152 strings)
        (srfi :262 match)

        (srfi :275 iri)
        (srfi :275 uri)
        (srfi :275 normalize)
        (srfi :275 path)
        (srfi :275 utils))

#| Inline internal utility procedures which aren't exported
   in the (srfi :275 xxx) namespace: |#

(define (flatten lists0)
    (let loop ([lists lists0] [rest '()])
      (match lists
        ['() rest]
        [(cons (list sg) tail)
         (cons sg (loop tail rest))]
        [(cons (cons a a*) tail)
         (cons a (loop (cons a* tail) rest))]
        [(cons head tail)
         (cons head (loop tail rest))]
        [_
         (error 'flatten "not a proper list" lists0)])))

(define (identity x) x)

(define (compose f . rest)
  (if (null? rest)
      f
      (let ([g (apply compose rest)])
	(lambda args
	  (call-with-values (lambda () (apply g args)) f)))))

(define uri-path-string (compose path->string uri-path))
(define iri-path-string (compose path->string iri-path))
(define uri-path-segments (compose path-segments uri-path))
(define iri-path-segments (compose path-segments iri-path))

(define (shift-percent-encoding k) (fx+ k 256))
(define (unshift-percent-encoding sk) (fx- sk 256))

(define (char->u8-list ch)
  (bytevector->u8-list
   (string->utf8 (string ch))))

(define (hex-digit n)
  (if (fx<? n 10)
      (fx+ 48 n)
      (fx+ 55 n)))

(define (percent-encoding->u8-list %enc)
  (call-with-values (lambda () (fxdiv-and-mod %enc 16))
    (lambda (char0 char1)
      (list #x25 (hex-digit char0) (hex-digit char1)))))

(define (percent-encoding->string %enc)
  (u8-list->string (percent-encoding->u8-list %enc)))

(define u8-list->string
  (compose utf8->string u8-list->bytevector))

#|
   The first six test suite sections are portable, and only depend on the
   exports defined in the (srfi :275 xxx) sub-libraries:
|#
(include "test/test-misc.scm")
(include "test/test-parse-segment.scm")
(include "test/test-escaped.scm")
(include "test/test-path.scm")
(include "test/test-relative.scm")
(include "test/test-setters.scm")

#|
   In contrast, the following two test suite section is not portable.

   The normalization procedures *must* escape any invalid characters in URIs
   and IRIs. The library design makes it very difficult to actually construct
   such an URI or IRI, so the means by which this is tested is not portable,
   as it relies on the internal constructors.
|#
(import (only (srfi :275 internal resource-identifier)
              resource-identifier-raw-scheme
              resource-identifier-raw-user
              resource-identifier-raw-host
              resource-identifier-port
              resource-identifier-path
              resource-identifier-raw-query
              resource-identifier-raw-fragment
              relative-reference-raw-user
              relative-reference-raw-host
              relative-reference-port
              relative-reference-path
              relative-reference-raw-query
              relative-reference-raw-fragment)
        (only (srfi :275 internal iri)
              make-raw-relative-iri make-raw-iri)
        (only (srfi :275 internal uri)
              make-raw-relative-uri make-raw-uri))

(include "test/test-normalize.scm")
