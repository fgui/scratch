(ns sc-specs
  (:require [clojure.spec.alpha :as s]))

;; playing with specs
;;(require '[clojure.spec.alpha :as s])

(s/conform even? 1000)
(s/valid? even? 1000)

(s/def ::day #{:mon :tue :wed :thu :fri :sat :sun})
(s/valid? ::day :fri)

(s/def ::name-or-id (s/or :name string?
                          :id   int?))

(s/conform ::name-or-id 12)
(s/conform ::name-or-id "12")
(s/conform ::name-or-id :keyword)
(s/explain ::name-or-id :keyword)
(def phone-regex #"^[0-9 ]*$")
(s/def ::phone (s/and string? #(re-matches phone-regex %)))
(s/valid? ::phone "93 666 66 66")
(s/def ::name string?)
(s/def ::lastname string?)

(s/def ::person (s/keys :req [::name ::lastname]
                        :opt [::phone]))

(s/valid? ::person
          {::name "Jorge"
           ::lastname "Jungle"})

;; extra-key is still valid
(s/valid? ::person
          {::name "Jorge"
           ::lastname "Jungle"
           ::extra-key "doesn't matter"})

;; non-qualified no valid
(s/valid? ::person
          {:name "Jorge"
           :lastname "Jungle"})

(s/def :unq/person (s/keys :req-un [::name ::lastname]
                           :opt-un [::phone]))

(s/valid? :unq/person
          {:name "Jorge"
           :lastname "Jungle"})

(defrecord Person [name lastname phone])

(s/valid? :unq/person (->Person "Jorge" "Jungle" "666 66"))

(s/def ::class string?)

(s/def ::teacher (s/merge
                  ::person
                  (s/keys :req [::class])))

(s/conform ::teacher {::name "Jorge"
                      ::lastname "Jungle"
                      ::class "Gym"})

(s/conform (s/coll-of
            keyword?
            :count 3
            :distinct true
            :into #{})
           [:a :b :c])
