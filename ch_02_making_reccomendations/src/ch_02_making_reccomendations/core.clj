(ns ch-02-making-reccomendations.core
  (:require clojure.set)
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn foobar [] "foobar")

;the critics
(def critics {
  "Lisa Rose"    {
    "Lady in the Water"  2.5
    "Snakes on a Plane"  3.5
    "Just My Luck"       3.0
    "Superman Returns"   3.5
    "You, Me and Dupree" 2.5
    "The Night Listener" 3.0
  }
  "Gene Seymour" {
    "Lady in the Water"  3.0
    "Snakes on a Plane"  3.5
    "Just My Luck"       1.5
    "Superman Returns"   5.0
    "The Night Listener" 3.0
    "You, Me and Dupree" 3.5
  }
  "Michael Phillips" {
    "Lady in the Water"   2.5
    "Snakes on a Plane"   3.0
    "Superman Returns"    3.5
    "The Night Listener"  4.0
  }
  "Claudia Puig" {
    "Snakes on a Plane"   3.5
    "Just My Luck"        3.0
    "The Night Listener"  4.5
    "Superman Returns"    4.0
    "You, Me and Dupree"  2.5
  }
  "Mick LaSalle" {
    "Lady in the Water"   3.0
    "Snakes on a Plane"   4.0
    "Just My Luck"        2.0
    "Superman Returns"    3.0
    "The Night Listener"  3.0
    "You, Me and Dupree"  2.0
  }
  "Jack Matthews" {
    "Lady in the Water"   3.0
    "Snakes on a Plane"   4.0
    "The Night Listener"  3.0
    "Superman Returns"    5.0
    "You, Me and Dupree"  3.5
  }
  "Toby" {
    "Snakes on a Plane"   4.5
    "You, Me and Dupree"  1.0
    "Superman Returns"    4.0
  }
})

(defn get-rating [prefs person key]
  (get (get prefs person) key))

(defn sum-of-squares
  "computes the sum of the squares"
  [diffrences]
  (reduce
    (fn [memo diffrence] (+ memo (* diffrence diffrence)))
    0
    diffrences))

(defn shared_keys
  "gets the keys shared between two maps"
  [& items]
  (apply clojure.set/intersection
    (map (fn [item]  (set (keys item))) items)))

(defn sim_distance
  "calculates the similarity distance between two records"
  [prefs person-1 person-2]
  (let [
      distance (sum-of-squares
                            (map (fn [key] (- (get-rating prefs person-2 key) (get-rating prefs person-1 key))) (shared_keys (get prefs person-1) (get prefs person-2))))
    ]
    (/ 1 (+ 1 distance))))