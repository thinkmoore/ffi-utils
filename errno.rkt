#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse
                     "private/generate-errno.rkt")
         racket/contract
         syntax/parse)

(provide (contract-out [errno? (-> any/c boolean?)]
                       [code->errno (-> integer? (or/c errno? #f))]
                       [errno->code (-> errno? integer?)]
                       [errno->message (-> errno? string?)])
         errno-symbols)

(define-syntax (define-maps stx)
  (define-values (syms codes messages)
    (generate-errno-tables))
  (syntax-parse stx
    [(define-maps sym->code code->sym sym->message code->message)
     (with-syntax ([(s ...) syms]
                  [(c ...) codes]
                  [(m ...) messages])
     #`(begin
         (define sym->code
           (make-hash (list (cons 's c) ...)))
         (define code->sym
           (make-hash (list (cons c 's) ...)))
         (define sym->message
           (make-hash (list (cons 's m) ...)))))]))

(define-maps sym->code code->sym sym->msg code->msg)

(define (errno? v)
  (hash-has-key? sym->code v))

(define (code->errno code)
  (hash-ref code->sym code (lambda () #f)))

(define (errno->code errno)
  (hash-ref sym->code errno))

(define (errno->message errno)
  (hash-ref sym->msg errno))

(define errno-symbols (hash-keys sym->code))
