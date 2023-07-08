#!/usr/bin/env racket

#lang racket

(require odysseus)
(require tabtree)
(require tabtree/utils)

(define unicorn (parse-tabtree "../unicorn_ontology.tree"))
(define products (->> unicorn hash-values (filter (λ (item) (consists-of? ($ a item) "Продукт")))))
(define tools (->> unicorn hash-values (filter (λ (item) (consists-of? ($ a item) "Инструмент" "РучнойИнструмент")))))
(--- "Entities:" (length (hash-keys unicorn)))
(--- "Hvor av")
(--- "Products:" (length products))
(--- "Tools:" (length tools))
