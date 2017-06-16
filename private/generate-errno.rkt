#lang racket/base

(provide generate-errno-tables)

(require racket/list
         ffi/unsafe
         "includes.rkt")

(define strerror
  (get-ffi-obj 'strerror #f
               (_fun _int -> _string)))

(define (extract-errno line)
  (define matched
    (regexp-match #rx"#[ \t]*define[ \t]+(E[^ \t]+)[ \t]+([0-9]+)" line))
  (and matched (list (string->number (third matched))
                     (string->symbol (second matched)))))

(define (parse-errno path)
  (define lines (get-system-include path))
  (if lines
      (filter-map (λ (line) (extract-errno line)) lines)
      empty))

(define (generate-errno-tables)
  (define errnos (append (parse-errno "sys/errno.h")
                         (parse-errno "bits/errno.h")
                         (parse-errno "linux/errno.h")
                         (parse-errno "asm/errno.h")
                         (parse-errno "asm-generic/errno-base.h")
                         (parse-errno "asm-generic/errno.h")
                         (parse-errno "errno.h")))
  (for/fold ([syms empty]
             [codes empty]
             [messages empty])
            ([errno (in-list errnos)])
    (define code (first errno))
    (define sym  (second errno))
    (define message (strerror code))
    (values (cons sym syms)
            (cons code codes)
            (cons message messages))))
