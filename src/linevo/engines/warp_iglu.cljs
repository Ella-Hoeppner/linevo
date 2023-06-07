(ns linevo.engines.warp-iglu
  (:require [sprog.util :as u]
            [linevo.rand :refer [rand-direction]]))

(defn get-op-generator [& [{:keys [rand-fn
                                   dimensions
                                   frequency-min
                                   frequency-max
                                   amplitude-modifier
                                   end-layer-chance]
                            :or {rand-fn rand
                                 end-layer-chance 0.25
                                 frequency-min 0.5
                                 frequency-max 40}}]]
  (fn []
    (if (< (rand-fn) end-layer-chance)
      {:op :end-layer}
      (cond-> {:op :warp
               :frequency (* frequency-min
                             (Math/pow (/ frequency-max frequency-min)
                                       (rand-fn)))
               :amplitude (cond-> (rand-fn)
                            amplitude-modifier amplitude-modifier)
               :phase (rand-fn)}
        (> dimensions 1) (merge
                          {:dot-direction
                           (rand-direction rand-fn dimensions)
                           :offset-direction
                           (rand-direction rand-fn dimensions)})))))

(defn generate-program [& [{:keys [dimensions
                                   max-length]
                            :or {dimensions 1
                                 max-length 10}}]]
  (vec (repeatedly max-length 
                   (get-op-generator {:dimensions dimensions
                                      :amplitude-modifier (partial * 0.25)}))))

(defn preprocess-program [program]
  (if (= (:op (last program)) :end-layer)
    program
    (conj program {:op :end-layer})))

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
         (mapcat (fn [{:keys [op] :as op-map}]
                   (case op
                     :warp
                     (let [{:keys [dot-direction
                                   offset-direction
                                   frequency
                                   amplitude
                                   phase]} 
                           op-map]
                       (u/log [dot-direction offset-direction])
                       '((+= offset
                             (-> x
                                 ~(list (if (= 1 dimensions) '* 'dot)
                                        (if (= 1 dimensions)
                                          (* u/TAU frequency)
                                          (cons x-type
                                                (map (partial * frequency)
                                                     dot-direction))))
                                 (+ ~(* u/TAU phase))
                                 sin
                                 (* ~(if (= 1 dimensions)
                                       amplitude
                                       (cons x-type
                                             (map (partial * amplitude)
                                                  offset-direction))))))))
                     :end-layer
                     '((+= x offset)
                       (= offset ~(if (= dimensions 0)
                                    0
                                    (list x-type 0))))))
                 (preprocess-program program))
         '(x))}}})))
