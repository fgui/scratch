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
     (clojure.string/join " " (map name (keys languages))))

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
                         (conj acc [1 val]))))
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
     nil))

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

(defprotocol FormatCase
  (format-case [data verb to-format]))

(extend clojure.lang.Keyword
  FormatCase
  {
   :format-case (fn [data _ to-format]
                  (->
                   data
                   name
                   extract-words
                   ((to-format format-functions))
                   keyword))})



(format-case :hello-koko :using :camel-case)

(clojure.test/deftest keyword-to-x-case-tests
  (clojure.test/are [x y] (= x y)
    (format-case :hello-koko :using :camel-case) :helloKoko
    (format-case :hello-koko :using :snake-case) :hello_koko
    (format-case :hello-koko :using :pascal-case) :HelloKoko
    (format-case :hello-koko :using :kebab-case) :hello-koko
    (format-case :helloKoko :using :kebab-case) :hello-koko))

(keyword-to-x-case-tests)

(extend clojure.lang.Keyword
  FormatCase
  {
   :format-case (fn [data _ to-format]
                  (->
                   data
                   name
                   extract-words
                   ((to-format format-functions))
                   keyword))})

(clojure.test/deftest keyword-to-x-case-box-tests
  (clojure.test/are [x y] (= x y) 
   (format-case [:hello-koko :hello_koko] :using :camel-case)  [:helloKoko :helloKoko]
   (format-case #{:hello-koko :hello_koko} :using :camel-case)  #{:helloKoko}
   (format-case {:hello-koko 1 :HelloLolo :hello_koko} :using :camel-case) {:helloKoko 1 :helloLolo :helloKoko}))

(keyword-to-x-case-box-tests)

(extend java.lang.Object
  FormatCase
  {
   :format-case (fn [this x to-format]
                  this)})

(extend clojure.lang.APersistentSet
  FormatCase
  {
   :format-case (fn [this x to-format]
                  (set (map #(format-case % x to-format) this)))})

(extend clojure.lang.APersistentVector
  FormatCase
  {
   :format-case (fn [this x to-format]
                  (mapv #(format-case % x to-format) this))})

(extend clojure.lang.APersistentMap
  FormatCase
  {
   :format-case (fn [this x to-format]
                  (into {} (map (fn [[k v]]
                                 [(format-case k x to-format)
                                  (format-case v x to-format)]) this)))})


(clojure.test/deftest keyword-to-x-case-recur-tests
  (clojure.test/are [x y] (= x y) 
    (format-case [:hello-koko :hello_koko #{:hello-koko :hello_koko}
                  {:hello-koko 1 :HelloLolo :hello_koko}]
                 :using :camel-case )  [:helloKoko :helloKoko #{:helloKoko} {:helloKoko 1 :helloLolo :helloKoko}]))

(keyword-to-x-case-recur-tests)

;; playing with a list of numbers 
;; find numbers that add up to a number

(def numbers-double [229.7 183.25 130.06 106.13 112.65 140.66 91.71 278.31 197.31 197.31 374.13 158.85 176.5 148.78 122.85 646.33 46.32 168.98 85.05 110.45 439.19 91.89 146.26 34.15 26.8 320.78 70.45 456.91 32.8 63.08 177.31 196.96 194.82 57.71 58.52 25.91 54.16 145.78 117.4 13.05 74.86 10.16 176.5 229.45 46.32 46.32 25.26 292.22 11.61 157.25 106 125 44.48 104.61 61.2 268.63 78.44 30.77 137.59 252.25 15.23 148.72 51.22 57.7 80.67 78.19 74.81 58.7 65.04 95.46 153.47 198.79 79.01 60.25 318.99 318.99 407.58 407.58 73.98 391.78 263.33 647.67 647.67 97.28 205.18 244.67 75.96 27.32 12.31 61.72 44.1 14.28 129.91 222.5 94.89 209.85 104.19 491.83 168.52 80.04 123.45 198.02 89.86 271.08 96 76.84 238.59 87.33 389.28 262.82 174.83 394.81 341.69 194.82 218.79 53.14 351.43 168.82 201.01 227.15 133.33 170.3 133.67 71.59 218.15 75.35 54.74 187.75 126.14 102.91 65.6 128.37 128.37 12.68 66.07 265.98 40.74 294.83 210.04 442.85 313.8 27.14 99.47 102.58 37.06 151.68 81.24 1100.57 130.39 394.81 93.28 45.23 118.04 140.98 343.24 403.1 41.31 133.85 133.85 57.86 171.39 171.39 75.7 75.7 91.89 116.49 134.6 146.63 448.45 229.9 132.07 107.24 25.26 34.15 36 639.41 136.14 80.51 583.93 159.55 31.81 98.77 67.76 205.77 1332.38 19.65 139.96 21.45 32.92 8.8 492.41 76.48 54.07 91.8 264.17 294.96 180.6 276.1 44.51 44.51 196.5 104.27 121.39 138.46 106.7 156.94 56.3 260.16 138.32 91.12 106.86 704.46 61.98 194.26 96.51 281.24 136.96 83.76 170.9 211.31 257.44 98.34 98.34 185.38 62.22 143.63 201.39 82.88 15.91 101.19 87.63 81.54 100.54 197.31 80.39 103.07 211.39 67.7 132.66 175.09 19.65 147.16 229.45 191.35 110.47 107.45 154.66 175.09 201.52 383.61 31.21 97.15 26.39 137.95 76.64 900.32 154.63 398.77 137.95 61.3 212 456.89 133.4 322 149.45 48.57 85.33 100.32 56.19 45.71 356.77 206.29 121.22 148.9 62.13 15.23 102.72 200.97 41.31 41.31 260.41 255.16 19.65 289.8 102.48 324.74 26.38 26.38 159.04 159.04 136.88 99.97 99.97 144.76 67.42 109 61.72 42.48 81.71 523.52 15.74 112.65 143.61 66.68 123.37 123.78 40.37 48.57 38.71 38.71 39.5 159.04 173.65 285.24 134.25 239.07 111.65 79.93 11.07 145.83 112.31 57.54 158.99 176.5 123.55 107.7 197.45 162.09 45.25 67.42 67.42 111.8 137.39 316.1 45.91 173.3 109.46 132.07 85.05 58.6 181.79 10.42 40.74 51.85 24.36 38.25 63.38 174.19 174.19 174.19 19.65 29.04 173.22 204.72 42.59 470.06 310.44 69.75 122.89 141.67 193.56 123.81 70.5 130.41 214.04 168.65 198.44 160.59 16.17 330.68 357.08 357.08 146.04 51.21 194.82 144.36 21.38 21.19 66.51 128.93 128.93 44.96 12.94 86.66 198.79 237.16 72.93 62.57 509.25 115.42 80.53 109.91 250.85 52.84 101.45 183.15 72.45 142.02 89.04 53.21 181.46 181.46 218.32 183.92 155.84 49.36 82.18 47.72 393.9 442.85 210.04 294.83 40.74 313.8 265.98 456.91 646.33 19.65 51.85 174.19 91.89 173.22 29.04 146.26 122.85 320.78 439.19 110.45 204.72 34.15 174.19 174.19 470.06 70.45 46.32 24.36 38.25 168.98 42.59 85.05 40.74 63.38 26.8 159.55 19.65 91.8 583.93 31.81 136.14 80.51 205.77 54.07 8.8 32.92 1332.38 639.41 67.76 98.77 139.96 76.48 21.45 492.41 147.16 191.35 110.47 27.14 99.47 104.27 137.95 76.64 154.63 900.32 398.77 212 81.71 36987.99])

(def numbers-big-decimal [229.7M 183.25M 130.06M 106.13M 112.65M 140.66M 91.71M 278.31M 197.31M 197.31M 374.13M 158.85M 176.5M 148.78M 122.85M 646.33M 46.32M 168.98M 85.05M 110.45M 439.19M 91.89M 146.26M 34.15M 26.8M 320.78M 70.45M 456.91M 32.8M 63.08M 177.31M 196.96M 194.82M 57.71M 58.52M 25.91M 54.16M 145.78M 117.4M 13.05M 74.86M 10.16M 176.5M 229.45M 46.32M 46.32M 25.26M 292.22M 11.61M 157.25M 106 125M 44.48M 104.61M 61.2M 268.63M 78.44M 30.77M 137.59M 252.25M 15.23M 148.72M 51.22M 57.7M 80.67M 78.19M 74.81M 58.7M 65.04M 95.46M 153.47M 198.79M 79.01M 60.25M 318.99M 318.99M 407.58M 407.58M 73.98M 391.78M 263.33M 647.67M 647.67M 97.28M 205.18M 244.67M 75.96M 27.32M 12.31M 61.72M 44.1M 14.28M 129.91M 222.5M 94.89M 209.85M 104.19M 491.83M 168.52M 80.04M 123.45M 198.02M 89.86M 271.08M 96M 76.84M 238.59M 87.33M 389.28M 262.82M 174.83M 394.81M 341.69M 194.82M 218.79M 53.14M 351.43M 168.82M 201.01M 227.15M 133.33M 170.3M 133.67M 71.59M 218.15M 75.35M 54.74M 187.75M 126.14M 102.91M 65.6M 128.37M 128.37M 12.68M 66.07M 265.98M 40.74M 294.83M 210.04M 442.85M 313.8M 27.14M 99.47M 102.58M 37.06M 151.68M 81.24M 1100.57M 130.39M 394.81M 93.28M 45.23M 118.04M 140.98M 343.24M 403.1M 41.31M 133.85M 133.85M 57.86M 171.39M 171.39M 75.7M 75.7M 91.89M 116.49M 134.6M 146.63M 448.45M 229.9M 132.07M 107.24M 25.26M 34.15M 36M 639.41M 136.14M 80.51M 583.93M 159.55M 31.81M 98.77M 67.76M 205.77M 1332.38M 19.65M 139.96M 21.45M 32.92M 8.8M 492.41M 76.48M 54.07M 91.8M 264.17M 294.96M 180.6M 276.1M 44.51M 44.51M 196.5M 104.27M 121.39M 138.46M 106.7M 156.94M 56.3M 260.16M 138.32M 91.12M 106.86M 704.46M 61.98M 194.26M 96.51M 281.24M 136.96M 83.76M 170.9M 211.31M 257.44M 98.34M 98.34M 185.38M 62.22M 143.63M 201.39M 82.88M 15.91M 101.19M 87.63M 81.54M 100.54M 197.31M 80.39M 103.07M 211.39M 67.7M 132.66M 175.09M 19.65M 147.16M 229.45M 191.35M 110.47M 107.45M 154.66M 175.09M 201.52M 383.61M 31.21M 97.15M 26.39M 137.95M 76.64M 900.32M 154.63M 398.77M 137.95M 61.3M 212M 456.89M 133.4M 322M 149.45M 48.57M 85.33M 100.32M 56.19M 45.71M 356.77M 206.29M 121.22M 148.9M 62.13M 15.23M 102.72M 200.97M 41.31M 41.31M 260.41M 255.16M 19.65M 289.8M 102.48M 324.74M 26.38M 26.38M 159.04M 159.04M 136.88M 99.97M 99.97M 144.76M 67.42M 109M 61.72M 42.48M 81.71M 523.52M 15.74M 112.65M 143.61M 66.68M 123.37M 123.78M 40.37M 48.57M 38.71M 38.71M 39.5M 159.04M 173.65M 285.24M 134.25M 239.07M 111.65M 79.93M 11.07M 145.83M 112.31M 57.54M 158.99M 176.5M 123.55M 107.7M 197.45M 162.09M 45.25M 67.42M 67.42M 111.8M 137.39M 316.1M 45.91M 173.3M 109.46M 132.07M 85.05M 58.6M 181.79M 10.42M 40.74M 51.85M 24.36M 38.25M 63.38M 174.19M 174.19M 174.19M 19.65M 29.04M 173.22M 204.72M 42.59M 470.06M 310.44M 69.75M 122.89M 141.67M 193.56M 123.81M 70.5M 130.41M 214.04M 168.65M 198.44M 160.59M 16.17M 330.68M 357.08M 357.08M 146.04M 51.21M 194.82M 144.36M 21.38M 21.19M 66.51M 128.93M 128.93M 44.96M 12.94M 86.66M 198.79M 237.16M 72.93M 62.57M 509.25M 115.42M 80.53M 109.91M 250.85M 52.84M 101.45M 183.15M 72.45M 142.02M 89.04M 53.21M 181.46M 181.46M 218.32M 183.92M 155.84M 49.36M 82.18M 47.72M 393.9M 442.85M 210.04M 294.83M 40.74M 313.8M 265.98M 456.91M 646.33M 19.65M 51.85M 174.19M 91.89M 173.22M 29.04M 146.26M 122.85M 320.78M 439.19M 110.45M 204.72M 34.15M 174.19M 174.19M 470.06M 70.45M 46.32M 24.36M 38.25M 168.98M 42.59M 85.05M 40.74M 63.38M 26.8M 159.55M 19.65M 91.8M 583.93M 31.81M 136.14M 80.51M 205.77M 54.07M 8.8M 32.92M 1332.38M 639.41M 67.76M 98.77M 139.96M 76.48M 21.45M 492.41M 147.16M 191.35M 110.47M 27.14M 99.47M 104.27M 137.95M 76.64M 154.63M 900.32M 398.77M 212M 81.71M 36987.99M])

(reduce + numbers-double)
(map inc numbers-double)

;; interesting things about doubles
(inc 255.16)
(+ 1 255.16)
(+ 0 256.16)

(reduce + numbers-big-decimal)

(type (first numbers-double))
(type (first numbers-big-decimal))

(def looking-for 199.25M)

(time (set (for [x numbers-big-decimal
                y numbers-big-decimal
                :when (= looking-for (+ x y))]
            (sort[x y]))))




;; this takes longer that I expected (42 seconds aprox) and it produces a lot of options.
(time 
 (set (for [x numbers-big-decimal
            y numbers-big-decimal
            z numbers-big-decimal
            :when (= looking-for (+ x y z))]
        (sort[x y z]))))


;; shortcutting a bit on the when takes 36 seconds
(time 
 (set (for [x numbers-big-decimal
            y numbers-big-decimal
            z numbers-big-decimal
            :when (and (< x looking-for)
                       (< (+ x y) looking-for)
                       (= looking-for (+ x y z)))]
        (sort[x y z]))))


(count numbers-big-decimal)
(count (filter #(<= % looking-for) numbers-big-decimal))

;; this version takes 164ms order is important in the for,
;; the if we do z and then x y; x, y will be calculated which
;; make me thing we should use let.
(time
 (for [[x y] (set (for [x numbers-big-decimal
                        y numbers-big-decimal
                        :when (>= looking-for (+ x y))]
                    (sort[x y])))
       z (filter #(<= % looking-for) numbers-big-decimal)
       :when (= looking-for (+ x y z))]
   (sort[x y z])))

;; yes using let is less time 140ms aprox.
(time
 (let [candidate-numbers (filter #(<= % looking-for) numbers-big-decimal)]
   (for [[x y] (set (for [x numbers-big-decimal
                          y numbers-big-decimal
                          :when (>= looking-for (+ x y))]
                      (sort[x y])))
         z candidate-numbers
         :when (= looking-for (+ x y z))]
     (sort[x y z]))))

;; finally since we have the let we can use candidates everywhere
(time
 (let [candidate-numbers (filter #(<= % looking-for) numbers-big-decimal)]
   (for [[x y] (set (for [x candidate-numbers
                          y candidate-numbers
                          :when (>= looking-for (+ x y))]
                      (sort[x y])))
         z candidate-numbers
         :when (= looking-for (+ x y z))]
     (sort[x y z]))))

;; introduce number 0 to have also the 1,2 or 3 digits.
(time
 (let [candidate-numbers (conj (filter #(<= % looking-for) numbers-big-decimal) 0)]
   (for [[x y] (set (for [x candidate-numbers
                          y candidate-numbers
                          :when (>= looking-for (+ x y))]
                      (sort[x y])))
         z candidate-numbers
         :when (= looking-for (+ x y z))]
     (sort[x y z]))))

;; sort results by first
(time
 (sort-by first
          (let [candidate-numbers (conj (filter #(<= % looking-for) numbers-big-decimal) 0)]
            (for [[x y] (set (for [x candidate-numbers
                                   y candidate-numbers
                                   :when (>= looking-for (+ x y))]
                               (sort[x y])))
                  z candidate-numbers
                  :when (= looking-for (+ x y z))]
              (sort[x y z])))))


;;test remove it doesn't conserve the "box" set returns a lazyseq.
(let [r (remove #{"env/dev"} #{"src" "env/dev"})]
  (println (type r))
  r)

