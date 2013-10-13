(ns retro.core)

(defn- exp 
  ([pow] (exp Math/E pow))
  ([base pow]
    (Math/pow base pow)))

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
  (let [row  (nth lattice j)]
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
  (nth (nth lattice i) j))

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
  (reduce + (map (partial * spin) nn-spins)))


(defn metropolis-step
  [lattice temp]
  (let [pos  (rand-pos lattice)
        spin (spin-at lattice pos)
        neighbours (neighbours-spins lattice pos)
        before (energy spin neighbours)
        after  (energy (flip spin) neighbours)
        delta (- before after)]
  (if (neg? delta)
    (flip! lattice pos)
    (if (< (rand) (exp (/ delta temp)))
      (flip! lattice pos)
      lattice))))


(defn simulate
  [lattice start stop]
  (loop [lattice (make-lattice 5 5) temp start]
    (println lattice)
    (if (< temp stop)
      (recur 
        (metropolis-step lattice temp)
        (+ 0.1 temp))
      lattice)))

(comment
  (def lattice (make-lattice 30 30))
  (spin-at lattice [3 3]) 
  (exp 6)
  (def lattice (make-lattice 5 5))
  (metropolis-step lattice 1)
  )
