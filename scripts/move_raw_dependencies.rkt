#!/usr/bin/env racket

#lang racket

(require odysseus)
(require tabtree)
(require tabtree/utils)
(require tabtree/output)

(define craft (parse-tabtree "../craft.tree"))

(define prods (->> craft hash-values (filter-map (λ (item) ($ prod item))) flatten remove-duplicates ((λ (x) (sort x a-z)))))

; (hash-filter (λ (k v) (index-of? '("fac" "raw" "nat-raw" "src" "mac" "tl" "spl") k)) (hash "raw" 1 "foo" 5))

; (define result-str
;   (->>
;        craft
;        (filter-tabtree
;          (λ (item)
;            (hash-filter (λ (k v) (index-of? '("fac" "raw" "nat-raw" "src" "mac" "tl" "spl") k)) item)))
;        ; tabtree->string
;        ))

(define prods-tt
  (for/fold
    ((res (hash)))
    ((prod prods))
    (let ((prod-item
            (->> craft
                 hash-values
                 (filter
                   (λ (item) (or
                                (equal? ($ prod item) prod)
                                (and (list? ($ prod item)) (index-of? ($ prod item) prod)))))
                 (map (λ (item)
                        (hash-filter (λ (k v) (index-of? '("fac" "raw" "nat-raw" "src" "mac" "tl" "spl") k)) item)))
                 (apply hash-union)
                 (hash-union (hash "__id" prod)))))
      (hash-union
        res
        (hash prod prod-item)))))

(write-file "../production_.tree" (tabtree->string #:pars-print-order "fac,nat-raw,raw,src,mac,tl,spl" prods-tt))
