(ns linevo.engines.warp-kudzu
  (:require [hollow.util :as u]
            [kudzu.tools :refer [unquotable]]
            [linevo.rand :refer [rand-fn-rand-nth
                                 rand-direction]]
            [kudzu.chunks.misc :refer [sympow-chunk]]
            [kudzu.core :refer [combine-chunks]]))

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
                                 end-layer-chance 0.25
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

(defn preprocess [program]
  (reduce (fn [new-program op]
            (if (= (:type op) :end-layer)
              (if (or (empty? new-program)
                      (= (:type (peek new-program)) :end-layer))
                new-program
                (conj new-program op))
              (conj new-program op)))
          []
          (conj program {:type :end-layer})))

(defn compile-program [program
                       & [{:keys [fn-name
                                  dimensions]
                           :or {fn-name 'twist
                                dimensions 1}}]]
  (combine-chunks
   sympow-chunk
   (unquotable
    (let [x-type ('[float vec2 vec3 vec4] (dec dimensions))]
      {:functions
       {fn-name
        (concat
         '(~x-type
           [x ~x-type]
           (= ~x-type offset ~(if (= dimensions 0) 'float (list x-type 0))))
         (map
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
                '(:block
                  (=float osc-pos
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
                  (=float osc-value
                          ~(case (:osc op-map)
                             :sin '(sin (* osc-pos ~u/TAU))
                             :saw '(uni->bi (mod osc-pos 1))
                             :pulse '(if (< (mod osc-pos 1)
                                            ~(:pulse-width op-map))
                                       -1
                                       1)
                             :sin-pow '(sympow (sin (* osc-pos ~u/TAU))
                                               ~(:power op-map))
                             :smooth-saw
                             (let [smoothness (:smoothness op-map)]
                               '(* (- 1
                                      (/ (* 2
                                            (acos
                                             (* (- 1 ~smoothness)
                                                (- (cos (* osc-pos
                                                           ~Math/PI))))))
                                         ~Math/PI))
                                   (* 2
                                      (/ (atan
                                          (/ (sin (* osc-pos ~Math/PI))
                                             ~smoothness))
                                         ~Math/PI))))))
                  (+= offset (* ~(if (= 1 dimensions)
                                   (* amplitude (first offset-direction))
                                   (cons x-type
                                         (map (partial * amplitude)
                                              offset-direction)))
                                osc-value))))
              :end-layer
              '(do (+= x offset)
                   (= offset ~(if (= dimensions 0)
                                0
                                (list x-type 0))))))
          program)
         '(x))}}))))
