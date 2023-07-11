(ns linevo.engines.float-unistack
  (:require [linevo.rand :refer [rand-fn-exponential
                                 rand-fn-geometric
                                 rand-fn-rand-nth]]
            [linevo.engines.shared.constant-generator
             :refer [get-numeric-constant-generator]]))

(def sigmoid (comp / inc #(Math/exp %) -))

(defn arity-safe-fn [arity f]
  (fn [stack]
    (if (>= (count stack) arity)
      (f stack)
      stack)))

(def environment
  (let [stack-fn (fn [arity f]
                   (fn [stack]
                     (if (>= (count stack) arity)
                       (cons (apply f (take arity stack))
                             (drop arity stack))
                       stack)))]
    (merge
     {:swap (arity-safe-fn 2 #(concat (reverse (take 2 %))
                                      (drop 2 %)))
      :swap-2 (arity-safe-fn 4 #(concat (take 2 (drop 2 %))
                                        (take 2 %)
                                        (drop 4 %)))
      :swap-3 (arity-safe-fn 6 #(concat (take 3 (drop 3 %))
                                        (take 3 %)
                                        (drop 6 %)))
      :drop rest
      :drop-2 (partial drop 2)
      :drop-3 (partial drop 3)
      :reverse reverse}
     (mapcat (fn [[wrapper key-fn-pairs]]
               (map (fn [[k f]]
                      [k (wrapper f)])
                    key-fn-pairs))
             {(partial stack-fn 1)
              {:negate -
               :inverse /
               :cos Math/cos
               :sin Math/sin
               :tan Math/tan
               :sinh Math/sinh
               :cosh Math/cosh
               :tanh Math/tanh
               :exp Math/exp
               :log Math/log
               :abs Math/abs
               :floor Math/floor
               :ceil Math/ceil
               :max Math/max
               :fract #(mod % 1)
               :sqrt Math/sqrt
               :sigmoid sigmoid}
              (partial stack-fn 2)
              {:+ +
               :- -
               :* *
               :/ /
               :min min
               :max max
               :pow Math/pow
               :mod mod}
              (partial arity-safe-fn 1)
              {:dup-1 #(conj % (first %))
               :dup-2 #(concat (repeat 2 (first %)) %)
               :dup-3 #(concat (repeat 3 (first %)) %)}
              (partial arity-safe-fn 2)
              {:dup-2-1 #(concat (take 2 %) %)
               :dup-2-2 #(concat (apply concat (repeat 2 (take 2 %))) %)
               :dup-2-3 #(concat (apply concat (repeat 3 (take 2 %))) %)}}))))

(defn eval-program [initial-values
                    program]
  (reduce (fn [values op]
            (if (number? op)
              (conj values op)
              ((environment op) values)))
          (seq initial-values)
          program))

(defn get-op-generator [& [{:keys [rand-fn
                                   ops
                                   constant-chance
                                   constant-options]
                            :or {rand-fn rand
                                 ops (vec (keys environment))
                                 constant-chance 0.1}}]]
  (let [constant-generator (get-numeric-constant-generator constant-options)]
    (fn []
      (if (< (rand-fn) constant-chance)
        (constant-generator)
        (rand-fn-rand-nth rand-fn ops)))))
