(ns linevo.gp
  (:require [linevo.rand :refer [rand-fn-rand-nth
                                rand-fn-geometric
                                rand-fn-shuffle]]))

(defn mutation [program
                op-generator
                & [{:keys [rand-fn
                           addition-rate
                           substitution-rate
                           deletion-rate]
                    :or {rand-fn rand
                         addition-rate 0.05
                         substitution-rate 0.05
                         deletion-rate 0.05}}]]
  (reduce (fn [new-program old-program-op]
            (cond-> new-program
              (>= (rand-fn) deletion-rate)
              (conj (if (>= (rand-fn) substitution-rate)
                      old-program-op
                      (op-generator)))
              (< (rand-fn) addition-rate)
              (conj (op-generator))))
          (if (< (rand-fn) addition-rate)
            [(op-generator)]
            [])
          program))

(defn crossover [parents
                 & [{:keys [rand-fn
                            alternation-rate
                            jump-rate
                            jump-p]
                     :or {rand-fn rand
                          alternation-rate 0.05
                          jump-rate 0.5
                          jump-p 0.5}}]]
  (let [parent-set (set parents)]
    (loop [new-program []
           index 0
           current-parent (rand-fn-rand-nth rand-fn (vec parent-set))]
      (let [alternate? (< (rand-fn) alternation-rate)
            new-parent (if alternate?
                         current-parent
                         (rand-fn-rand-nth rand-fn (vec parent-set)))]
        (if (>= index (count new-parent))
          new-program
          (recur (conj new-program (new-parent index))
                 (if (and alternate? (< (rand-fn) jump-rate))
                   (max 0
                        (+ index
                           1
                           (* (if (> (rand-fn) 0.5) 1 -1)
                              (inc (rand-fn-geometric rand-fn jump-p)))))
                   (inc index)) 
                 new-parent))))))

(defn breed [op-generator
             parents
             & [{:keys [rand-fn
                        mutation-chance
                        mutation-options
                        post-alternation-mutation-chance
                        post-alternation-mutation-options
                        alternation-options
                        alternation-population-size-p]
                 :or {rand-fn rand
                      mutation-chance (/ 3)
                      post-alternation-mutation-chance 0.5
                      alternation-population-size-p 0.5}}]]
  (if (or (= 1 (count parents))
          (< (rand-fn) mutation-chance))
    (mutation (rand-fn-rand-nth rand-fn (vec parents))
              op-generator
              (assoc mutation-options
                     :rand-fn
                     rand-fn))
    (cond-> (crossover
             (take (+ 2 (rand-fn-geometric rand-fn
                                           alternation-population-size-p))
                   (rand-fn-shuffle rand-fn
                                    (seq parents)))
             (assoc alternation-options
                    :rand-fn
                    rand-fn))
      (< (rand-fn) post-alternation-mutation-chance)
      (mutation op-generator
                (assoc post-alternation-mutation-options
                       :rand-fn
                       rand-fn)))))
