(ns linevo.rand)

(defn rand-fn-rand-int
  ([rand-fn high]
   (Math/floor (* (rand-fn) high)))
  ([rand-fn low high]
   (+ low (rand-fn-rand-int rand-fn (- high low)))))

(defn rand-fn-rand-nth [rand-fn v]
  (nth v (Math/floor (* (count v) (rand-fn)))))

(defn rand-fn-geometric [rand-fn p]
  (loop [x 0]
    (if (< (rand-fn) p) x (recur  (inc x)))))

(defn rand-fn-exponential [rand-fn lambda]
  (/ (- (Math/log (rand-fn))) lambda))

(defn rand-fn-shuffle [rand-fn coll]
  (loop [remaining-values coll
         shuffled-list ()]
    (let [size (count remaining-values)]
      (if (pos? size)
        (let [index (Math/floor (* size (rand-fn)))]
          (recur (concat (take index remaining-values)
                         (drop (inc index) remaining-values))
                 (conj shuffled-list (nth remaining-values index))))
        shuffled-list))))

(defn rand-box-muller [rand-fn]
  (let [angle (* Math/PI 2 (rand-fn))]
    (mapv (comp (partial * (Math/sqrt (* -2 (Math/log (rand-fn)))))
                #(% angle))
          [Math/cos Math/sin])))

(defn rand-normals
  ([rand-fn] (apply concat (repeatedly (partial rand-box-muller rand-fn))))
  ([rand-fn n] (take n (rand-normals rand-fn))))

(letfn [(normalize [v]
          (let [mag (Math/sqrt (apply + (map #(* % %) v)))]
            (mapv #(/ % mag)
                  v)))]
  (defn rand-direction [rand-fn dimensions]
    (normalize (rand-normals rand-fn dimensions))))