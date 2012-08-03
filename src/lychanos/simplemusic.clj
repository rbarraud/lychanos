(ns lychanos.simplemusic
  (:require [clojure.math.combinatorics :as combinatorics])
  (:use [clojure.string :only (join split)]))

(defn mod12 [x]
  (mod x 12))

(defn interval [x y]
  (mod12 (- x y)))

(defn interval-class [x y]
  (min (interval x y) (interval y x)))

(defn rotate [item & [n]]
  (let [n (if n n 1)
        modn (mod n (count item))]
    (concat (subvec (vec item) modn)
            (subvec (vec item) 0 modn))))

(defn rotate-set [notes]
  (map #(rotate notes %1) (range 0 (count notes))))

(defn intervals [notes]
  (map interval-class notes (butlast (rotate notes))))

(defn all-intervals [notes]
  (mapcat intervals (combinatorics/combinations (sort notes) 2)))

(defn transposition [notes index]
  (map #(mod12 (+ %1 index)) notes))

(defn transposition-startswith [notes start]
  (transposition notes (- start (first notes))))

(defn inversion [notes & [index]]
  (let [index (if index index 0)]
    (map #(mod12 (- index %1)) notes)))

(defn inversion-startswith [notes start]
  (transposition-startswith (inversion (transposition-startswith notes 0)) start))

(defn inversion-first-note [notes]
  (inversion notes (* 2 (first notes))))

(defn retrograde [notes]
  (reverse notes))

(defn note-name [number]
  (let [notes (clojure.string/split "C C# D D# E F F# G G# A A# B" #"\s+")]
    (nth notes (mod12 number))))

(defn notes-names [notes]
  (map note-name notes))
