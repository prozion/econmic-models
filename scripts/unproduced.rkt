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

(define production (parse-tabtree "../production.tree"))

(define raws (all-unique-values production "raw"))
(define prods (->> production hash-keys))
(define tools (all-unique-values production "tl"))
(define gars (all-unique-values production "gar"))
(define macs (all-unique-values production "mac"))
(define supplies (all-unique-values production "spl"))

(--- "Unproduced raws:" (minus raws (join prods tools gars)))
(--- "Unproduced supplies:" (minus supplies (join prods gars)))
(--- "Unproduced tools:" (minus tools (join prods gars)))
(--- "Unproduced machines:" (minus macs prods))
