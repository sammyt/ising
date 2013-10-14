(ns retro.core
  (:require
    [c2.core]
    [goog.dom :as dom]
    [cljs.core.async :refer [>! <! chan timeout alts!]])
  (:require-macros 
    [c2.util :refer [bind! pp]]
    [cljs.core.async.macros :refer [go]]))

(defn- exp 
  ([pow] (Math/exp pow)))

(defn- row 
  "helper function to generate vector of random 1s and -1s"
  [i] 
  (vec (take i (repeatedly 
                 #(let [i (rand-int 2)] (if (zero? i) -1 i))))))

(defn- flip
  [spin]
  (* -1 spin))

(defn flip!
  [lattice [i j]]
  (let [row (nth lattice j)]
    (assoc lattice j
      (assoc row i (flip (nth row i))))))

(defn make-lattice 
  "build a lattice of dimension i by j"
  [i j] 
  (vec (take j (repeatedly #(row i)))))

(defn rand-pos
  "generate a random position within a lattice"
  [lattice]
  (let [j (int (count lattice))
        i (int (count (first lattice)))]
    [(rand-int i) (rand-int j)]))

(defn spin-at
  "find the spin at a given lattice position"
  [lattice [i j]]
  (nth (nth lattice j) i))

(defn neighbours-spins 
  "find the spins of the nearest neighbours"
  [lattice [i j]]
  (let [h (count lattice)
        w (count (first lattice))]
    (map 
      (partial spin-at lattice) 
      [[i (mod (inc j) h)]
       [i (mod (dec j) h)]
       [(mod (inc i) w) j]
       [(mod (dec i) w) j]])))

(defn energy
  [spin nn-spins]
  (* 2 spin (reduce + nn-spins)))

(defn total-energy 
  [lattice]
  (reduce + (flatten lattice)))

(defn metropolis-step
  [lattice temp]
  (let [pos  (rand-pos lattice)
        spin (spin-at lattice pos)
        neighbours (neighbours-spins lattice pos)
        before (energy spin neighbours)
        after  (energy (flip spin) neighbours)
        delta (- before after)]
    (if (or (<= delta 0) (< (rand) (exp (- (/ delta temp)))))
      (flip! lattice pos)
      lattice)))

(defn simulate-at-temp
  ([lattice temp] 
    (simulate-at-temp lattice temp (count lattice)))
  ([lattice temp iterations]
    (loop [lattice lattice i 0]
      (if (< i iterations)
        (recur 
          (metropolis-step lattice temp)
          (inc i))
        lattice))))


(bind! 
  "div.ising" 
  [:div [:canvas.draw {:height (str 500 "px")
                       :width (str 500 "px")}]])

(defn draw-rect
  [ctx [x y] [h w]]
  (.fillRect ctx x y h w))

(defn draw-spin
  [ctx size spin pos]
  (doall 
    (if (neg? spin)
      (aset ctx "fillStyle" "#ff0000")
      (aset ctx "fillStyle" "#00ff00"))
    (draw-rect ctx pos size)))

(defn draw-lattice
  [lattice draw]
  (map-indexed 
    (fn [j r] 
      (doall 
        (map-indexed
          (fn [i s]
            (draw s [(* j 21) (* i 21)] [20 20]))
          r)))
    lattice))

(go
  (let [canvas (dom/getElementByClass "draw")
        ctx (.getContext canvas "2d") ]
    (loop [lattice (make-lattice 30 30)]
      (<! (timeout 5))
      (doall 
        (draw-lattice
          lattice
          (partial draw-spin ctx [20 20])))
      (recur (simulate-at-temp lattice 1 100)))))

