#lang racket/base

(provide define-enum provide-enum
         define-bitmask provide-bitmask)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/template)
         ffi/unsafe
         (only-in racket/contract or/c listof))

(begin-for-syntax
  (define-splicing-syntax-class bit-definer
    #:literals (=)
    (pattern (~seq name:id = val:nat))
    (pattern name:id)))

(define-syntax (define-enum stx)
  (syntax-parse stx
    [(_ name:id (def:bit-definer ...) type:id)
     (with-syntax ([cname (format-id #'name "_~a" #'name)]
                   [pred  (format-id #'name "~a?" #'name)]
                   [symbols (format-id #'name "~a-symbols" #'name)])
       (template
        (begin
          (define cname (_enum '((?@ . def) ...) type))
          (define (pred v)
            (member v '(def.name ...)))
          (define symbols '(def.name ...)))))]))

(define-syntax (provide-enum stx)
  (syntax-parse stx
    [(_ name:id)
     (with-syntax ([pred (format-id #'name "~a?" #'name)]
                   [symbols (format-id #'name "~a-symbols" #'name)])
       #'(provide pred symbols))]))

(define-syntax (define-bitmask stx)
  (syntax-parse stx
    [(_ name:id (def:bit-definer ...) type:id)
     (with-syntax ([cname (format-id #'name "_~as" #'name)]
                   [pred  (format-id #'name "~a?" #'name)]
                   [ctc   (format-id #'name "~as/c" #'name)]
                   [symbols (format-id #'name "~a-symbols" #'name)])
       (template
        (begin
          (define cname (_bitmask '((?@ . def) ...) type))
          (define (pred v)
            (member v '(def.name ...)))
          (define ctc (or/c pred (listof pred)))
          (define symbols '(def.name ...)))))]))

(define-syntax (provide-bitmask stx)
  (syntax-parse stx
    [(_ name:id)
     (with-syntax ([pred (format-id #'name "~a?" #'name)]
                   [ctc  (format-id #'name "~as/c" #'name)]
                   [symbols (format-id #'name "~a-symbols" #'name)])
       #'(provide pred ctc symbols))]))
