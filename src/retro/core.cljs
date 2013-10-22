(ns retro.core
  (:require
    [goog.dom :as dom]
    [cljs.core.async :refer [>! <! chan timeout alts!]])
  (:require-macros 
    [cljs.core.async.macros :refer [go]]))

(def not-nil? (complement nil?))

(defn- lattice-neighbours
  "Calculates the nearest neighbours within a 2d infinite lattice"
  [[x y]]
  (for [[dx dy] #{[-1 0] [1 0] [0 -1] [0 1]}] 
    [(+ x dx) (+ y dy)]))

(defn- positive-spins 
  "generates randomly positioned positive spins within a 2d space"
  ([] (positive-spins 20 20))
  ([w h] 
   (set (for [x (range w) y (range h) :when (< (rand) 0.5)] [x y]))))

(defn- cyclic
  "enforce cyclic boundaries on positions" 
  [positions w h]
  (map (fn [[x y]] [(mod x w) (mod y h)]) positions))

(defn- boltzmann
  "calculate the boltzmann factor"
  [energy-delta temperature]
  (Math/exp (- (/ energy-delta temperature))))

(defn- energy-delta
  "calculate the change in energy should a spin be flipped"
  [positives neighbours]
  (+ -4 (* 2 (count (filter not-nil? (map positives neighbours))))))

(defn- metropolis-step
  "for a given temperature, progress the system on by one monte-carlo step"
  [width height temperature positives]
  (let [pos [(rand-int width) (rand-int height)]
        spin (if (contains? positives pos) 1 -1)
        neighbours (cyclic (lattice-neighbours pos) width height)
        delta (* spin (energy-delta positives neighbours))] 
    (if (or (neg? delta) 
            (< (rand) (boltzmann delta temperature)))
      (if (contains? positives pos) 
        (disj positives pos) 
        (conj positives pos))
      positives)))


(defn- context-2d
  [el]
  (.getContext el "2d"))

(defn- draw-rect
  ([ctx x y] 
   (draw-rect ctx x y 20 20))
  ([ctx x y w h]
   (.fillRect ctx x y w h)))

(let [ctx (context-2d (dom/getElement "out"))]
  (defn- render
    [positions]
    (do
      (.clearRect ctx 0 0 400 400)
      (doall (for [[x y] positions] 
               (draw-rect ctx (* 20 x) (* 20 y)))))))

#_(go 
  (let [width 20 height 20 
        temperature 1
        positives (positive-spins width height)
        step (partial metropolis-step width height)]
    (loop [positives positives i 0] 
      (<! (timeout 5))
      (when (< i 10000)
        (render positives)
        (recur (step temperature positives) (inc i))))))



