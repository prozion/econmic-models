#!/usr/bin/env racket

#lang racket

(require odysseus)
(require tabtree)
(require tabtree/utils)

(define (entity? s)
  (re-matches? "[А-Я].*" s))

(define production (parse-tabtree "../products.tree"))
(define products (parse-tabtree "../products.tree"))
(define natural-resources (parse-tabtree "../natural_resources.tree"))
(define craft (parse-tabtree "../crafts.tree"))
(define process (parse-tabtree "../process.tree"))

(define unicorn (->>
                  (parse-tabtree "../unicorn_ontology.tree")
                  (filter-tabtree (λ (k v) (and
                                              (not (statement? v))
                                              (not (index-of? '("namespaces" "classes" "properties" "individuals") k))
                                              (not (index-of? '("rdf/Property" "owl/Ontology" "ЛинияПроизводства") ($ a v)))
                                              (not (index-of? '("namespaces" "properties" "individuals") ($ __parent v)))
                                              )))
                                              ))

(define classes (->> unicorn hash-values (filter (λ (item) (index-of? (flatten (list ($ a item))) "owl/Class"))) (map (λ (item)  ($ __id item)))))
(define alts (->> unicorn hash-values (filter-map (λ (item) ($ alt item))) flatten cleanmap remove-duplicates))
(define defined-ids (append
                      (remove-duplicates
                        (append
                          (hash-keys products)
                          (hash-keys natural-resources)
                          (hash-keys craft)
                          (hash-keys process)))
                      classes))
(define all-ids (->> unicorn hash-keys))
(define all-refs (->> unicorn hash-values (map hash-values) flatten remove-duplicates (filter-not hash?) (filter-not string-in-string?) (filter entity?)))

(define undefined-ids (minus (join all-ids all-refs) (join defined-ids alts)))

; (--- unicorn)

(if (empty? undefined-ids)
  (--- "No undefined entities")
  (begin
    (--- "Undefined entities:")
    (---- undefined-ids)))

; (--- (->> unicorn hash-values (filter (λ (item) (equal? ($ __id item) "Производитель")))))
; (->> unicorn hash-values (filter (λ (item) (index-of? (flatten (list ($ a item))) "owl/Class"))) (map (λ (item)  ($ __id item))))
; (--- (->> (parse-tabtree "../unicorn_ontology.tree") hash-values (filter (λ (item) (equal? ($ __id item) "Производитель")))))
