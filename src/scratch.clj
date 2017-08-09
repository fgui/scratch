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

;;<clojure.walk>

(require '[clojure.walk :as w])

;; similar to a map + function on result
(w/walk #(* % 10) #(apply + %) [1 2 3])

(w/prewalk #(do
              (println %)
              (if (number? %) (inc %) %))
           [[1 2 3]
            [4 5 6]
            [7 8 9]])

;; on postwalk the no leaf nodes have been modified on visit
(w/postwalk #(do
              (println %)
              (if (number? %) (inc %) %))
            [[1 2 3]
             [4 5 6]
             [7 8 9]])



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
;; binary identity associative

(m/mappend (m/mappend [1 2] [3 4]) [5 6])
(m/mappend [1 2] (m/mappend [3 4] [5 6]))
(m/mappend [] [1 2])
(m/mappend [1 2] [])

(m/mappend (maybe/just [1 2 3])
           (maybe/nothing))

(m/mappend (maybe/nothing)
           (maybe/just [4 5 6]))

(m/mappend (maybe/just [1 2 3])
           (maybe/nothing)
           (maybe/just [4 5 6])
           (maybe/nothing))

;; *functor*
;; struture (list maybe...)
;; preserves struture
;; identity
;; composition

(m/fmap inc [1 2])
(m/fmap inc [])
;; composition
(m/fmap (comp str inc) [1 2])
(m/fmap str (m/fmap inc [1 2]))

;; maybe is a functor.
;; (fmap [f fv])

(maybe/just 2)
(m/fmap inc (maybe/just 1))
(m/fmap inc (maybe/nothing))

(m/fmap inc {:key1 1 :key2 2})
(map (fn [[k v]] [k v]) {:key1 1 :key2 2})

;; *applicative*

;; (fapply [af av])
;; (pure [v])  (pure [ctx v])
(doc m/pure)

(cats.context/with-context maybe/context (m/pure 1))

(m/fapply [inc #(* % 10)] [1 2 3])
(m/fapply [] [1 2 3])
(m/fapply [inc #(* % 10)] [])

(m/fapply (maybe/just (fn [i] (inc i))) (maybe/just 1))
(m/fapply (maybe/just (fn [i] (inc i))) (maybe/nothing))
(m/fapply (maybe/nothing) (maybe/just 1))


;; *foldable foldl foldr*
(m/foldl (fn [acc v] (+ acc v)) 0 [1 2 3 4 5])
(m/foldr (fn [v wc] (+ v wc)) 0 [1 2 3 4 5])

;; foldm - aware of monad context function
(m/foldm (fn [x y] (maybe/just (+ x y))) 0 [1 2 3])

;; traversal
;; structures that can be traversed from left to right running
;; an applicative action for each element.

(defn just-if-even [n]
  (if (even? n)
    (maybe/just n)
    (maybe/nothing)))

(require '[cats.context :as ctx])

(ctx/with-context maybe/context
  (m/traverse just-if-even []))

(ctx/with-context maybe/context
  (m/traverse just-if-even [1 2]))

(ctx/with-context maybe/context
  (m/traverse just-if-even [4 2]))

;; monads
;; bind [mv f]  --> like functor but with inverted parameters
;; return
(m/bind (maybe/just 1)
        (fn [v] (maybe/just (inc v))))

(m/bind (maybe/just 1)
        (fn [v] (m/return (inc v))))

(m/bind (maybe/just 1)
        (fn [a] (m/bind (maybe/just (inc a))
                        (fn [b] (m/return (* b 2))))))

(m/mlet [a (maybe/just 1)
        b (maybe/just (inc a))]
        (m/return (* b 2)))

;; identity value of a monad will shortcut

(m/mzero maybe/context)

(m/mlet [a (maybe/just 1)
         :when (= a 2)]
        (m/return (* a 2)))
(m/mlet [a (maybe/just 2)
         :when (= a 2)]
        (m/return (* a 2)))

;; monad plus supports associative binary operation

;; mplus for maybe similar to or
(m/mplus (maybe/nothing) (maybe/just 1))
(m/mplus (maybe/just 2) (maybe/just 1))


(m/bind [1 2 3]
        (fn [v] (m/mzero)))

(m/bind [1 2 3]
        (fn [v] (m/return (inc v))))

(m/mplus [1] [2 3])

;; deref maybe
(maybe/from-maybe (maybe/just 1))
@(maybe/just 1)
@(maybe/nothing)

;; fmap <$>
(m/<$> inc [1 2 3])
(m/fmap inc [1 2 3])

;;aplicative functor with alet
(m/alet [a (maybe/just [1 2 3])
         b (maybe/just [4 5 6])]
        (m/mappend a b))


;; exceptions
(require '[cats.monad.exception :as exc])


(exc/try-on 1)

(exc/try-on (+ 1 nil))

(exc/try-or-else (+ 1 nil) 2)

(exc/try-or-recover (+ 1 nil)
                    (fn [e]
                      (cond
                        (instance? NullPointerException e) 0
                        :else 100)))

(exc/try-or-recover (/ 1 0)
                    (fn [e]
                      (cond
                        (instance? NullPointerException e) 0
                        :else 100)))


(def f (exc/try-on (+ 1 nil)))
(m/fmap inc f)
@f  ;;throws the exception

(def f (exc/try-on (inc 0)))
(m/fmap inc f)
@f  ;;throws the exception


;; </funcool>

;; <look and say>

(defn next-step [s]
  (flatten (reduce (fn [acc val]
                     (let [[num-elems x] (last acc)]
                       (if (= val x)
                         (assoc acc (dec (count acc)) [(inc num-elems) x])
                         (conj acc [1 val]))
                       )
                     )
                   [[1 (first s)]] (rest s))))

(take 10 (iterate next-step [1]))

;; </look and say>

;; <macro>
(defmacro square [x]
  (list '* x x))

(defmacro square [x]
  `(let [y# ~x]
     (when (number? y#)
       (* y# y#))))


(macroexpand '(square (rand-int 10)))
(macroexpand-1 '(square (rand-int 10)))
(clojure.walk/macroexpand-all '(square (rand-int 10)))

(square (rand-int 10))


(defmacro when* [test & body]
  `(if ~test
     (do
       ~@body)
     nil
     ))

(when* (even? 3) (println 3) 3)
(when* (even? 2) (println 2) 2)

(macroexpand '(when* (even? x) (println x) x))

;; https://gist.github.com/trikitrok/a97d330bacb1f56fe5ee027c12ff273a
(require '[clojure.test :as test])

(def format-functions
  {:snake-case (fn [words]
                 (clojure.string/join "_" words))
   :kebab-case (fn [words]
                 (clojure.string/join "-" words))
   :camel-case (fn [words]
                 (apply str (first words)
                        (map clojure.string/capitalize (rest words))))
   :pascal-case (fn [words]
                 (apply str
                        (map clojure.string/capitalize words)))})

(defn extract-words [a-str]
  (map clojure.string/lower-case
       (filter #(not (#{"-" "_"} %))
               (re-seq #"[A-Z]?[a-z]+|-|_" a-str))))

(defn format [key _ to-format]
  (->
   key
   name
   extract-words
   ((to-format format-functions))
   keyword))


(test/deftest keyword-to-x-case-tests
  (test/are [x y] (= x y)
    (format :hello-koko :using :camel-case) :helloKoko
    (format :hello-koko :using :snake-case) :hello_koko
    (format :hello-koko :using :pascal-case) :HelloKoko
    (format :hello-koko :using :kebab-case) :hello-koko
    (format :helloKoko :using :kebab-case) :hello-koko))

(keyword-to-x-case-tests)


