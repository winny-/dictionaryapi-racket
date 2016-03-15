#lang racket/base

(require net/http-client
         net/uri-codec
         racket/contract
         racket/list
         racket/port
         racket/string
         xml)

(provide query-collegiate
         query-thesaurus)

(define/contract (parse-headers headers)
  (-> (listof bytes?) (or/c (hash/c string? string?)))
  (for/hash ([h headers])
    (apply values (map bytes->string/utf-8 (rest (regexp-match "([^:]*): (.*)" h))))))

(define/contract (parse-status-code status)
  (-> bytes? integer?)
  (string->number (cadr (string-split (bytes->string/utf-8 status)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (query-collegiate word key)
  (string? string? . -> . (values boolean? (or/c xexpr? string?)))
  (query (string-append "/references/collegiate/xml/" word) key))

(define/contract (query-thesaurus word key)
  (string? string? . -> . (values boolean? (or/c xexpr? string?)))
  (query (string-append "/references/thesaurus/xml/" word) key))

(define/contract (query method key)
  (string? string? . -> . (values boolean? (or/c xexpr? string?)))
  (define-values (status headers-list ip)
    (http-sendrecv "www.dictionaryapi.com"
                   (string-append "/api/v1" method "?key=" (uri-encode key))))
  (define code (parse-status-code status))
  (define headers (parse-headers headers-list))
  (define b (port->bytes ip))
  (define (handler e)
    (values #f (bytes->string/utf-8 b)))
  (if (= code 200)
      (with-handlers ([exn:fail? handler])
        (parameterize ([permissive-xexprs #t])
          (values #t (xml->xexpr (document-element (read-xml (open-input-bytes b)))))))
      (handler #f)))
