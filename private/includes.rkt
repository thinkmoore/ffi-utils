#lang racket/base

(provide get-system-include)

(require racket/port
         racket/list
         racket/match
         racket/string
         racket/system)

(define clang-command "clang -x c -v -E /dev/null")
(define gcc-command "echo | gcc -Wp,-v -x c - -fsyntax-only")

(define (get-system-include-paths/command command)
  (define output
    (call-with-output-string
     (λ (p) (parameterize ([current-output-port p]
                           [current-error-port p])
              (system command)))))
  (define search-list
    (string-split
     (second (regexp-match #rx"\n#include <\\.\\.\\.> search starts here:\n(.*)\nEnd of search list.\n" output))))
  (filter-map
   (λ (str)
     (let ([path (string->path str)])
       (and (directory-exists? path) path)))
   search-list))

(define (get-system-include-paths)
  (or (get-system-include-paths/command clang-command)
      (get-system-include-paths/command gcc-command)))

(define (get-system-include path)
  (for/or ([include-path (in-list (get-system-include-paths))])
    (with-handlers ([exn:fail:filesystem?
                     (λ (e) #f)])
      (call-with-input-file* (build-path include-path path)
        (λ (p) (port->lines p))))))
