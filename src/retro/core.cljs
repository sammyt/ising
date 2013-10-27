(ns retro.core
  (:require
    [goog.dom :as dom]
    [cljs.core.async :refer [>! <! chan timeout alts!]])
  (:require-macros 
    [cljs.core.async.macros :refer [go]]))

(def not-nil? (complement nil?))

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

(defn- neighbours-2d 
  "Calculates the nearest neighbours within a 2d infinite lattice"
  [[x y]]
  (for [[dx dy] #{[-1 0] [1 0] [0 -1] [0 1]}] 
    [(+ x dx) (+ y dy)]))

(defn- neighbours-3d 
  "Calculates the nearest neighbours within a 3d infinite lattice"
  [[x y z]]
  (for [[dx dy dz] 
        #{[-1 0 0] [1 0 0] [0 0 -1] [0 0 1] [0 -1 0] [0 1 0]}] 
    [(+ x dx) (+ y dy) (+ z dz)]))


(defn- hot-lattice 
  "generates randomly positioned positive spins within a 2d space"
  ([w h] 
   (set (for [x (range w) y (range h) :when (< (rand) 0.5)] [x y])))
  ([w h d] 
   (set (for [x (range w) y (range h) z (range d) :when (< (rand) 0.5)] [x y]))))

(defn- cyclic
  "enforce cyclic boundaries on positions" 
  ([positions w h] 
   (map (fn [[x y]] [(mod x w) (mod y h)]) positions))
  ([positions w h d] 
   (map (fn [[x y z]] [(mod x w) (mod y h) (mod z d)]) positions)))



#_(let [width 50 
      height 50 
      temp 2
      rand-pos (fn [] [(rand-int width) (rand-int height)])
      neighbours #(cyclic (neighbours-2d %) width height)
      step (partial metropolis-step rand-pos temp neighbours)]
   (go
    (loop [lattice (hot-lattice width height) i 0]
      (render lattice)
      (<! (timeout 10))
      (when (< i 100)
        (recur (last (take 50 (iterate step lattice))) (inc i))))))


(defn- pos
  [obj x y z] 
  (set! (.-x (.-position obj)) x) 
  (set! (.-y (.-position obj)) y) 
  (set! (.-z (.-position obj)) z)
  obj)

(defn- sphere
  [r]
  (js/THREE.Mesh.  
    (js/THREE.SphereGeometry. r 10 10) 
    (js/THREE.MeshLambertMaterial. (js-obj "color" 0x444488))))

(defn- light [] (js/THREE.PointLight. 0xffffff))

(let [aspect 1
      near 0.1
      far 10000
      renderer (js/THREE.WebGLRenderer. (js-obj "canvas" (dom/getElement "out3d")))
      camera (js/THREE.PerspectiveCamera. 45 aspect near far)
      scene (js/THREE.Scene.)
      positions (map #(- 30 (* 12 %)) (range 5))]

  (.add scene (pos (light)  80 30 80))
  (.add scene (pos (light) -80 30 -80))
  (.add scene camera)

  (doall (for [x positions  y positions z positions] 
           (.add scene (pos (sphere 4) x y z))))
             
  (defn render-3d [time-stamp]
    (let [angle (/ time-stamp 3600)]
      (pos camera (* 110 (Math/sin angle)) 60 (* 110 (Math/cos angle)))
      (.lookAt camera (.-position scene))
      (.render renderer scene camera)
      (js/requestAnimationFrame render-3d)))

  (render-3d (.getTime (js/Date.)))

  #_(let [width 20 
        height 20 
        depth 20
        temp 2
        rand-pos (fn [] [(rand-int width) (rand-int height) (rand-int depth)])
        neighbours #(cyclic (neighbours-3d %) width height depth)
        step (partial metropolis-step rand-pos temp neighbours)] 
    (go 
      (loop [lattice (hot-lattice width height depth) i 0]
        (.render renderer scene camera)
        (<! (timeout 10))
        (when (< i 1)
          (recur (step lattice) (inc i)))))))                  


