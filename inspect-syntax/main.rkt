#lang racket/base

(require macro-debugger/util/stxobj ; warning: private module
         racket/format
         racket/pretty)

(provide inspect-syntax)

;; This module provides utility functions for examining syntax objects

(define rule (make-string 50 #\;))
(define (prule) (displayln rule))

(define (inspect-syntax stx)
  (define datum (syntax->datum stx))
  (define keys (syntax-property-symbol-keys stx))

  (prule)
  (displayln (~a ";; Inspecting syntax object: " (~a stx #:max-width 20)))
  (newline)
  (displayln "Pretty-printed syntax:")
  (pretty-print datum)
  (newline)
  (displayln (~a "Source: "   (~a #:width 20 #:align 'right (syntax-source stx))))
  (displayln (~a "Line: "     (~a #:width 22 #:align 'right (syntax-line stx))))
  (displayln (~a "Column: "   (~a #:width 20 #:align 'right (syntax-column stx))))
  (displayln (~a "Position: " (~a #:width 18 #:align 'right (syntax-position stx))))
  (displayln (~a "Span: "     (~a #:width 22 #:align 'right (syntax-span stx))))
  (newline)
  (displayln "Properties:")
  (for ([key (in-list keys)])
    (displayln (~a "  " key ": " (syntax-property stx key))))
  (newline)
  (printf "Marks: ~a~n" (get-marks stx))
  (prule))
