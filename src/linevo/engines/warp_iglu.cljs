(ns linevo.engines.warp-iglu
  (:require [sprog.util :as u]
            [linevo.rand :refer [rand-fn-rand-int
                                 rand-fn-rand-nth
                                 rand-direction]]))

(defn specify-warp-op [op & [{:keys [dimensions
                                     rand-fn]
                              :or {dimensions 1
                                   rand-fn rand}}]]
  (merge op
         {:phase (rand-fn)
          :offset-direction
          (rand-direction rand-fn dimensions)}
         (when (> dimensions 1)
           {:dot-direction
            (rand-direction rand-fn dimensions)})))

(defn get-op-generator [& [{:keys [rand-fn
                                   dimensions
                                   frequency-min
                                   frequency-max
                                   amplitude-modifier
                                   end-layer-chance]
                            :or {rand-fn rand
                                 end-layer-chance 0.25
                                 frequency-min 0.25
                                 frequency-max 10}}]]
  (fn []
    (if (< (rand-fn) end-layer-chance)
      {:type :end-layer}
      (specify-warp-op
       {:type :warp
        :osc (rand-fn-rand-nth rand-fn [:sin :saw])
        :frequency (* frequency-min
                      (Math/pow (/ frequency-max frequency-min)
                                (rand-fn)))
        :amplitude (cond-> (rand-fn)
                     amplitude-modifier amplitude-modifier)}
       {:rand-fn rand-fn
        :dimensions dimensions}))))

(defn respecify-program [program & [options]]
  (mapv (fn [op]
          (if (= (:type op) :warp)
            (specify-warp-op op options)
            op))
        program))

(defn generate-program [& [{:keys [dimensions
                                   min-length
                                   max-length
                                   rand-fn
                                   op-generator-options]
                            :or {rand-fn rand
                                 dimensions 1
                                 min-length 70
                                 max-length 80}}]]
  (vec (repeatedly (rand-fn-rand-int rand-fn min-length max-length)
                   (get-op-generator (merge
                                      op-generator-options
                                      {:rand-fn rand-fn
                                       :dimensions dimensions})))))

(defn preprocess-program [program]
  (if (= (:type (last program)) :end-layer)
    program
    (conj program {:type :end-layer})))

(defn compile-program [program
                       & [{:keys [fn-name
                                  dimensions]
                           :or {fn-name 'twist
                                dimensions 1}}]]
  (u/unquotable
   (let [x-type ('[float vec2 vec3 vec4] (dec dimensions))]
     {:functions
      {fn-name
       {'([~x-type] ~x-type)
        (concat
         '([x]
           (= ~x-type offset ~(if (= dimensions 0) 'float (list x-type 0))))
         (mapcat (fn [op-map]
                   (case (:type op-map)
                     :warp
                     (let [{:keys [dot-direction
                                   offset-direction
                                   frequency
                                   amplitude
                                   phase]}
                           op-map]
                       '((+= offset
                             ~(concat
                               '(-> x
                                    ~(list (if (= 1 dimensions) '* 'dot)
                                           (if (= 1 dimensions)
                                             frequency
                                             (cons x-type
                                                   (map (partial * frequency)
                                                        dot-direction))))
                                    (+ ~phase))
                               (case (:osc op-map)
                                 :sin '((* ~u/TAU)
                                        sin)
                                 :saw '((mod 1)))
                               '((* ~(if (= 1 dimensions)
                                       (* amplitude (first offset-direction))
                                       (cons x-type
                                             (map (partial * amplitude)
                                                  offset-direction)))))))))
                     :end-layer
                     '((+= x offset)
                       (= offset ~(if (= dimensions 0)
                                    0
                                    (list x-type 0))))))
                 (preprocess-program program))
         '(x))}}})))
