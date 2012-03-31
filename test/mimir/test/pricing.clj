(ns mimir.test.pricing
  (:use [mimir.well :only (rule facts fact run)]
        [mimir.test.common]
        [clojure.test]))

(with-reset-fixture)

(defrecord Order [items customer total])
(defrecord OrderItem [description partNumber price quantity]
  Object
  (toString [_] (format "%d %s: %.2f" quantity description price)))
(defrecord CatalogItem [description partNumber price])
(defrecord Customer [orderCount])
(defrecord Offer [description amount]
  Object
  (toString [_] (format "%s: -$%.2f" description amount)))

(defn order [items customer]
  (let [total (reduce + (map #(* (:quantity %)
                                 (:price %)) items))]
    (Order. items customer total)))

(deftest pricing-1
  (facts (order [(OrderItem. "CD Writer" 1234 199.99 1)
                 (OrderItem. "AA Batteries" 4323 4.99 2)]
                (Customer. 6)))

  (rule a-10-percent-volume-discount
        "Give a 10% discount to everybody who spends more than $100."
        (> (.total ?o) 100)
        =>
        (fact (Offer. "10% volume discount" (/ (.total ?o) 10))))

  (match? (Offer. "10% volume discount" 20.997)))

(deftest pricing-2
  (facts (OrderItem. "Gold-tipped cable" 9876 19.99 4))

  (rule a-25-percent-multi-item-discount
    "Give a 25% discount on items the customer buys three or more of."
    (>= (.quantity ?item) 3)
    =>
    (fact (Offer. "25% multi-item discount" (/ (.price ?item) 4))))

  (match? (Offer. "25% multi-item discount" 4.9975)))

(deftest pricing-3
  (facts (CatalogItem. "CD Writer" 1234 199.99)
         (CatalogItem. "CD-RW Disks" 782321 5.99)
         (OrderItem. "CD Writer" 1234 199.99 1)
         (Customer. 6))

  (rule free-cd-rw-disks
    "If somebody buys a CD writer, send them a free sample of CD-RW
    disks, catalog number 782321; but only if they're a repeat customer.
    We use a regular expression to match the CD writer's description."
    (re-find #"CD Writer" (.description (cast CatalogItem ?catalog-item)))
    (= 782321 (.partNumber ?catalog-item-2))
    (= (.partNumber (cast OrderItem ?order-item))
       (.partNumber ?catalog-item))
    (> (.orderCount ?customer) 1)
    =>
    (fact (Offer. "Free CD-RW disks" (.price ?catalog-item-2))))

  (match? (Offer. "Free CD-RW disks" 5.99)))
