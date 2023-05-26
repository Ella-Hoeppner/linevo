(ns linevo.rand)

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
