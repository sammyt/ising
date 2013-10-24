(ns retro.core
  (:require
    [goog.dom :as dom]
    [cljs.core.async :refer [>! <! chan timeout alts!]])
  (:require-macros 
    [cljs.core.async.macros :refer [go]]))

(def not-nil? (complement nil?))

(defn- neighbours-2d 
  "Calculates the nearest neighbours within a 2d infinite lattice"
  [[x y]]
  (for [[dx dy] #{[-1 0] [1 0] [0 -1] [0 1]}] 
    [(+ x dx) (+ y dy)]))

(defn- lattice-2d 
  "generates randomly positioned positive spins within a 2d space"
  [w h] 
   (set (for [x (range w) y (range h) :when (< (rand) 0.5)] [x y])))

(defn- cyclic-2d
  "enforce cyclic boundaries on positions" 
  [positions w h]
  (map (fn [[x y]] [(mod x w) (mod y h)]) positions))

(defn- weighting 
  "calculate the boltzmann factor"
  [energy-delta temperature]
  (Math/exp (- (/ energy-delta temperature))))

(defn- energy
  "calculate the change in energy should a spin be flipped"
  [positives neighbours]
  (+ -4 (* 2 (count (filter not-nil? (map positives neighbours))))))

(defn metropolis-step 
  "progress a lattice on by one metropolis step"
  [rand-pos temp neighbours lattice]
  (let [pos (rand-pos)
        spin (if (contains? lattice pos) 1 -1)
        delta (* spin (energy lattice (neighbours pos)))]
    (if (or (neg? delta)
            (< (rand) (weighting delta temp)))
      (if (contains? lattice pos)
        (disj lattice pos)
        (conj lattice pos))
      lattice)))
        

(defn- context-2d
  [el]
  (.getContext el "2d"))

(defn- draw-rect
  ([ctx x y] 
   (draw-rect ctx x y 8 8))
  ([ctx x y w h]
   (.fillRect ctx x y w h)))

(let [ctx (context-2d (dom/getElement "out"))]
  (defn- render
    [positions]
    (do
      (.clearRect ctx 0 0 400 400)
      (doall (for [[x y] positions] 
               (draw-rect ctx (* 8 x) (* 8 y)))))))


(let [width 50 
      height 50 
      temp 2
      rand-pos (fn [] [(rand-int width) (rand-int height)])
      neighbours #(cyclic-2d (neighbours-2d %) width height)
      step (partial metropolis-step rand-pos temp neighbours)]
   (go
    (loop [lattice (lattice-2d width height) i 0]
      (render lattice)
      (<! (timeout 10))
      (when (< i 100)
        (recur (last (take 50 (iterate step lattice))) (inc i))))))                  

