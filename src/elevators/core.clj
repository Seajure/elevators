(ns elevators.core
  (:require [clojure.core.async :as async :refer [<! >! <!! >!! timeout chan alt! alts!! go]]))

(def out *out*)

(defn println* [& args]
  (binding [*out* out]
    (apply (partial println (java.util.Date.)) args)))

(defn start-cabin [->cabin ->shaft]
  (go
   (loop [floor 1]
     (go
      (>! ->shaft [:arrived floor]))
     (let [msg (<! ->cabin)]
       (condp = (first msg)
         :up
         (do
           (<! (timeout 1000))
           (recur (inc floor)))

         :down
         (do
           (<! (timeout 1000))
           (recur (dec floor)))

         :wait
         (do
           (<! (timeout 1000))
           (recur floor))

         :open
         (do
           (println* "open" floor)
           (<! (timeout 5000))
           (println* "closed" floor)

           (recur floor)))))))

(defn start-shaft [->shaft ->cabin]
  (println* "starting shaft, before go")
  (go
   (println* "starting shaft, in go")
   (loop [goals #{}]
     (let [msg (<! ->shaft)]
       (condp = (first msg)
         :up
         (let [floor (second msg)
               new-goals (conj goals floor)]
           (println* "goals:" new-goals ", floor:" floor)
           (recur new-goals))

         :arrived
         (let [floor (second msg)]
           (if (goals floor)
             (do
               (>! ->cabin [:open])
               (recur (disj goals floor)))
             (let [dest (first goals)]
               (cond
                (and dest (> dest floor))
                (do (>! ->cabin [:up]) (recur (disj goals floor)))

                (and dest (< dest floor))
                (do (>! ->cabin [:down]) (recur (disj goals floor)))

                :else
                (do
                  (>! ->cabin [:wait])
                  (recur goals)))))))))))

(defn main [->shaft]
  (let [->cabin (chan)
        cabin->cabin (chan)]
    (start-cabin ->cabin ->shaft)
    (start-shaft ->shaft ->cabin)))

(defn simulate []
  (let [->shaft (chan)]
    (main ->shaft)
    (async/thread
     (>!! ->shaft [:up 1])
     (>!! ->shaft [:up 2]))))
