(ns sc-core-async
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

;; https://www.braveclojure.com/core-async/

(def echo-chan (chan))
(go (println (<! echo-chan)))
(>!! echo-chan "ketchup")

(def echo-buffer-2 (chan 2))
;; it will not block on the first 2 calls
(>!! echo-buffer-2 "ketchup1")
(>!! echo-buffer-2 "ketchup2")
(go (println (<! echo-buffer-2))
    (println (<! echo-buffer-2)))

;; sliding-buffer fifo 
;; dropping-buffer lifo

;; parking (go !) vs blocking (thread !!)
;; thread (avoid parking use blocking)

(thread (println (<!! echo-chan)))
(>!! echo-chan "mustard")

;;thread acts as a channel??
(let [t (thread "chili")]
  (<!! t))

;;thread do a lot before working with channel b4 parking.

(defn hot-dog-machine [hot-dog-count]
  (let [in (chan) out (chan)]
    (go (loop [hc hot-dog-count]
          (if (> hc 0)
            (let [input (<! in)]
              (if (= 3 input)
                (do
                  (>! out "hot dog")
                  (recur (dec hc)))
                (do
                  (>! out "res de res")
                  (recur hc))))
            (do (close! in)
                (close! out)))))
    [in out]))

(let [[in out] (hot-dog-machine 2)]
  (>!! in "pocket lint")
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (<!! out))

;;channels as pipes
(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (go (>! c2 (clojure.string/upper-case (<! c1))))
  (go (>! c3 (clojure.string/reverse (<! c2))))
  (go (println (<! c3)))
  (>!! c1 "redrum"))

;;alts!!

(defn upload
  [headshot c]
  (go (Thread/sleep (rand 100))
      (>! c headshot)))

(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (upload "serious.jpg" c1)
  (upload "fun.jpg" c2)
  (upload "sassy.jpg" c3)
  (let [[headshot channel] (alts!! [c1 c2 c3])]
    (println "Sending headshot notification for" headshot)))


(let [c1 (chan)]
  (upload "serious.jpg" c1)
  (let [[headshot channel] (alts!! [c1 (timeout 40)])]
    (if headshot
      (println "Sending headshot notification for" headshot)
      (println "Timed out!"))))

;;specify operation on alts
;;example c2 put instead of take

(let [c1 (chan)
      c2 (chan)]
  (go (<! c2))
  (let [[value channel] (alts!! [c1 [c2 "put!"]])]
    (println value)
    (= channel c2)))


;; escape callback hell with pipelines

(defn upper-caser
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/upper-case (<! in)))))
    out))

(defn reverser
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/reverse (<! in)))))
    out))

(defn printer
  [in]
  (go (while true (println (<! in)))))

(def in-chan (chan))
(def upper-caser-out (upper-caser in-chan))
(def reverser-out (reverser upper-caser-out))
(printer reverser-out)

(>!! in-chan "redrum")
(>!! in-chan "repaid")
