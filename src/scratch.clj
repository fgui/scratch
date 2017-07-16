;; <basic core> -- one a day
;; https://www.clojure.org/api/cheatsheet

;; <documentation>
(doc doc)
(doc find-doc)
(find-doc "partition")
(apropos "partition")
(apropos "match")
(doc dir)
(dir user)
(dir clojure.set)
(/ 1 0)
;; print stack trace
(pst *e)
;; opens in browser
(javadoc "java.util.Date")

;; cider C-c C-d options
;; </documentation>



;; </basic core>

;; languages -- boot.build

(def lang "es")

(def languages
  {:ca "vocabularies/ca"
   :es "vocabularies/es"
   :eu "vocabularies/eu"
   :fr "vocabularies/fr"
   :en-uk "vocabularies/en-uk"
   :en-us "vocabularies/en-us"})

(str "language options: "
     (clojure.string/join " "(map name (keys languages))))

;; 2d matrix -- flat

(def sample-matrix [[1 2 3]
                [4 5 6]])

(def f-matrix (flatten sample-matrix))

f-matrix

(defn coordinates->flat-idx [row-size [x y]]
  (+ x (* y row-size)))

(defn flat-idx->coordinates [row-size flat-idx]
  [(quot flat-idx row-size) (mod flat-idx row-size)])

(flat-idx->coordinates 3 2)

(defn get [f-matrix row-size coordinates]
  (nth f-matrix
       (coordinates->flat-idx row-size coordinates)))

(def neighbord-coordinates [[0 1] [0 -1] [1 0] [-1 0]])

(defn neighbords [max-x max-y [x y]]
  (filter (fn [[a b]] (and (< -1 a max-x) (< -1 b max-y)))
   (map (fn [[inc-x inc-y]] [(+ x inc-x) (+ y inc-y)])
        neighbord-coordinates)))

(map (partial coordinates->flat-idx 3)
 (neighbords 4 4 [0 0]))

(and
 (= (get f-matrix 3 [1 0]) 2)
 (= (get f-matrix 3 [1 1]) 5)
 (= (get f-matrix 3 [0 1]) 4))


;; playing with specs
(require '[clojure.spec.alpha :as s])

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


;; <test>
;; playing with test
(require '[clojure.test :as test])

(test/deftest blah-test-fail
  (test/testing "blah not equals 1"
    (test/is (= "blah" 1))))

(blah-test-fail)


(test/deftest test-are
  (test/are [x y] (= 5 (+ x y))
    1 4
    2 3
    0 5))

(test-are)

;;run all test
(test/run-all-tests)

;; </test>

;;<funcool> for exceptions.

;;C-c C-m hh p ap add project dependency seems only works with leiningen.
(require '[cats.core :as m])
(require '[cats.builtin])



;; *semigroup* is an algebraic structure with an associative binary operation (mappend)

(m/mappend [1 2] [3 4 5])

(require '[cats.monad.maybe :as maybe])

(m/mappend (maybe/just [1 2 3])
           (maybe/just [4 5 6]))

;; *monoid* is a Semigroup with an identity element (mempty).
;; in the following examtles maybe/nothing is the identity element.

(m/mappend (maybe/just [1 2 3])
           (maybe/nothing))

(m/mappend (maybe/nothing)
           (maybe/just [4 5 6]))

(m/mappend (maybe/just [1 2 3])
           (maybe/nothing)
           (maybe/just [4 5 6])
           (maybe/nothing))

;; *functor*

;; maybe is a functor.
;; (fmap [f fv])

(maybe/just 2)
(m/fmap inc (maybe/just 1))
(m/fmap inc (maybe/nothing))

;; *applicative*

;; (fapply [af av])
;; (pure [v])  (pure [ctx v])
(doc m/pure)

(cats.context/with-context maybe/context (m/pure 1))

(m/fapply (maybe/just (fn [i] (inc i))) (maybe/just 1))
(m/fapply (maybe/just (fn [i] (inc i))) (maybe/nothing))
(m/fapply (maybe/nothing) (maybe/just 1))


;; *foldable foldl foldr*

;; </funcool>
