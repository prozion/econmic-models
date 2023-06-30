#!/usr/bin/env racket

#lang racket

(require odysseus)
(require tabtree)
(require tabtree/utils)

(define (all-unique-values tt key)
  (->> tt
       hash-values
       (filter-map (λ (item) (hash-ref item key #f)))
       flatten
       remove-duplicates
       ))

(define craft_tt (parse-tabtree "../craft.tree"))

(define raws (all-unique-values craft_tt "raw"))
(define supplies (all-unique-values craft_tt "spl"))
(define tools (all-unique-values craft_tt "tl"))
(define prods (all-unique-values craft_tt "prod"))

(--- "Pure raws:" (minus raws (join prods tools)))
(--- "Pure prods:" (minus (join prods tools) raws))
