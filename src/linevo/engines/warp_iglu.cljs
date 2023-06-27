(ns linevo.engines.warp-iglu
  (:require [sprog.util :as u]
            [linevo.rand :refer [rand-fn-rand-int
                                 rand-fn-rand-nth
                                 rand-direction]]
            [sprog.iglu.chunks.misc :refer [sympow-chunk]]
            [sprog.iglu.core :refer [gensym-replace
                                     combine-chunks]]))

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
            (rand-direction rand-fn dimensions)
            :radial-center
            (u/genv dimensions (- 1 (* 2 (rand-fn))))})))

(defn get-op-generator [& [{:keys [rand-fn
                                   dimensions
                                   frequency-min
                                   frequency-max
                                   amplitude-modifier
                                   end-layer-chance
                                   osc-types
                                   radial-chance
                                   sin-pow-distribution
                                   smooth-saw-distribution]
                            :or {rand-fn rand
                                 end-layer-chance 0.5
                                 frequency-min 0.8
                                 frequency-max 20
                                 radial-chance 0.5
                                 sin-pow-distribution
                                 #(Math/pow 3 (- (* 2 %) 1))
                                 smooth-saw-distribution
                                 #(* 0.1 (Math/pow 0.1 %))
                                 osc-types #{:sin
                                             :saw
                                             :pulse
                                             :sin-pow
                                             :smooth-saw}}}]]
  (let [osc-type-vec (vec osc-types)]
    (fn []
      (if (< (rand-fn) end-layer-chance)
        {:type :end-layer}
        (specify-warp-op
         (let [osc-type (rand-fn-rand-nth rand-fn osc-type-vec)]
           (merge
            {:type :warp
             :osc osc-type
             :frequency (* frequency-min
                           (Math/pow (/ frequency-max frequency-min)
                                     (rand-fn)))
             :amplitude (cond-> (rand-fn)
                          amplitude-modifier amplitude-modifier)}
            (case osc-type
              :pulse {:pulse-width (rand-fn)}
              :sin-pow {:power (sin-pow-distribution (rand-fn))}
              :smooth-saw {:smoothness (smooth-saw-distribution (rand-fn))}
              nil)
            (when (and (> dimensions 1)
                       (< (rand-fn) radial-chance))
              {:radial? true})))
         {:rand-fn rand-fn
          :dimensions dimensions})))))

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
                                 min-length 10
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
  (combine-chunks
   sympow-chunk
   (u/unquotable
    (let [x-type ('[float vec2 vec3 vec4] (dec dimensions))]
      {:functions
       {fn-name
        {'([~x-type] ~x-type)
         (concat
          '([x]
            (= ~x-type offset ~(if (= dimensions 0) 'float (list x-type 0))))
          (mapcat
           (fn [op-map]
             (case (:type op-map)
               :warp
               (let [{:keys [dot-direction
                             offset-direction
                             frequency
                             amplitude
                             phase
                             radial?
                             radial-center]}
                     op-map]
                 (gensym-replace
                  [:osc-pos]
                  '((=float :osc-pos
                            (+ ~phase
                               ~(if (= 1 dimensions)
                                  (list '* frequency)
                                  (if radial?
                                    '(* ~frequency
                                        (distance ~(cons x-type
                                                         (seq radial-center))
                                                  x))
                                    '(dot ~(cons x-type
                                                 (map (partial * frequency)
                                                      dot-direction))
                                          x)))))
                    (+= offset
                        (* ~(if (= 1 dimensions)
                              (* amplitude (first offset-direction))
                              (cons x-type
                                    (map (partial * amplitude)
                                         offset-direction)))
                           ~(case (:osc op-map)
                              :sin '(sin (* :osc-pos ~u/TAU))
                              :saw '(uni->bi (mod :osc-pos 1))
                              :pulse '(if (< (mod :osc-pos 1)
                                             ~(:pulse-width op-map))
                                        -1
                                        1)
                              :sin-pow '(sympow (sin (* :osc-pos ~u/TAU))
                                                ~(:power op-map))
                              :smooth-saw
                              (let [smoothness (:smoothness op-map)]
                                '(* (- 1 (/ (* 2
                                               (acos
                                                (* (- 1 ~smoothness)
                                                   (- (cos (* :osc-pos
                                                              ~Math/PI))))))
                                            ~Math/PI))
                                    (* 2
                                       (/ (atan (/ (sin (* :osc-pos ~Math/PI))
                                                   ~smoothness))
                                          ~Math/PI))))))))))
               :end-layer
               '((+= x offset)
                 (= offset ~(if (= dimensions 0)
                              0
                              (list x-type 0))))))
           (preprocess-program program))
          '(x))}}}))))
