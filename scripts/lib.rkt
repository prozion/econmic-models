#lang racket

(require odysseus)
(require tabtree)

(define products (parse-tabtree "../production.tree"))
(define prices (hash))
(define statements (->> products hash-values (filter (λ (item) (equal? ($ a item) "rdf/Statement")))))

(define (get-unit product-id)
  (->
    (hash-ref product-id (hash))
    (hash-ref "u" "i")))

(define (get-raw-in-product-share product-id raw-id)
  (let* ((product (hash-ref products product-id (hash)))
        (raw (hash-ref products raw-id (hash)))
        (raw-unit ($ u raw))
        (statement (filter
                      (λ (item)
                        (and
                          (equal? ($ rdf/subject item) product-id)
                          (equal? ($ rdf/predicate item) "raw")
                          (equal? ($ rdf/object item) raw-id)))
                      statements)))
    (if (empty? statement)
      0
      (->number (hash-ref (first statement) raw-unit 0)))))

(define (get-price product-id #:trace (trace #f))
  (or
    (hash-ref prices product-id #f)
    (let* ((product (hash-ref products product-id #f))
          (raw-ids (listify ($ raw product)))
          (raws-prices (->> raw-ids
                            (map
                              (λ (raw-id)
                                (*
                                  (get-price raw-id #:trace trace)
                                  (get-raw-in-product-share product-id raw-id))))))
          (hours (/ (->number (or ($ h product) 0))
                    (->number (or ($ p product) 1))
                    1.0))
          (price (+ hours (apply + raws-prices)))
          )
      (set! prices (hash-union prices (hash product-id price)))
      (when trace (--- product-id hours "+ [" (implode raw-ids ", ") "] =" price))
      price)))

(get-price "Лодка" #:trace #t)
