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

; note: the original code did not perform the sqrt. this did not make sense
; as the preceeding code hinted at needing it so I put it in.
(defn sim_distance
  "calculates the similarity distance between two records"
  [prefs person-1 person-2]
  (let [
      distance (Math/sqrt (sum-of-squares
                            (map
                              (fn [key] (- (get-rating prefs person-2 key) (get-rating prefs person-1 key)))
                              (shared_keys (get prefs person-1) (get prefs person-2)))))
    ]
    (/ 1 (+ 1 distance))))


(defn extract-values
  "returns the values for the keys as a sequence"
  [prefs person keys]
  (let [
        ratings (get prefs person)
        values (map #(get ratings %1) (keys ratings))
        ]
    values))

(defn pull-values
  "pulls the values out of the person from a set"
  [keyset prefs person]
  (map
    (fn [obj]
      (get (get prefs person) obj))
    keyset))

(defn sum  [coll] (reduce #(+ %1 %2) coll))

(defn square-sum [values]
  (sum
    (map #(* %1 %1)  values)))

(defn sim-pearson
  "calculates the pearson score between two reveiwers"
  [prefs person-1 person-2]
  (let [
        shared (shared_keys (get prefs person-1) (get prefs person-2))
        p1-values (pull-values shared prefs person-1)
        p2-values (pull-values shared prefs person-2)
        p1-sum (sum p1-values)
        p2-sum (sum p2-values)
        p1-square-sum (square-sum p1-values)
        p2-square-sum (square-sum p2-values)
        p-sum (sum (map
                     (fn [[arg1 arg2]] (* arg1 arg2))
                     (map vector p1-values p2-values)) )
        num (- p-sum (/ (* p1-sum p2-sum) (count shared)))
        den (Math/sqrt (*
                         (- p1-square-sum (/ (Math/pow p1-sum 2) (count shared) ))
                         (- p2-square-sum (/ (Math/pow p2-sum 2) (count shared) ))
                         ))
      ] (cond
                 (<= (count shared) 0)  0
                 (= den 0) 0
                 :else (/ num den))))
