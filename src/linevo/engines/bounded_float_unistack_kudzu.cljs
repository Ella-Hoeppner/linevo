(ns linevo.engines.bounded-float-unistack-kudzu
  (:require [clojure.walk :refer [prewalk
                                  prewalk-replace]]
            [linevo.rand :refer [rand-fn-rand-nth
                                 rand-fn-exponential]]
            [sprog.util :as u]))

(defn get-numeric-constant-generator [& [{:keys [rand-fn
                                                 lambda]
                                          :or {rand-fn rand
                                               lambda 0.5}}]]
  (fn []
    (* (if (< (rand-fn) 0.5) -1 1)
       (/ (inc (rand-fn-exponential rand-fn lambda))))))

(def environment
  (prewalk-replace
   {:TAU (* Math/PI 2)}
   '{:+ [2 -1
         (= [stack -1]
            (clamp (+ [stack 0] [stack -1])
                   -1
                   1))]
     :- [2 -1
         (= [stack -1]
            (clamp (- [stack 0] [stack -1])
                   -1
                   1))]
     :* [2 -1 (= [stack -1] (* [stack 0] [stack -1]))]
     :mod [2 -1 (= [stack -1] (mod [stack 0] [stack -1]))]
     :pow [2 -1
           (= [stack -1]
              (* (sign [stack 0])
                 (pow (abs [stack 0])
                      (if (>= [stack -1] 0)
                        [stack -1]
                        (/ (- [stack -1]))))))]
     :min [2 -1 (= [stack -1] (min [stack 0] [stack -1]))]
     :max [2 -1 (= [stack -1] (max [stack 0] [stack -1]))]
     :drop [2 -1]
     :negate [1 0 (=-> [stack 0] -)]
     :sin [1 0 (=-> [stack 0] (* :TAU) sin)]
     :cos [1 0 (=-> [stack 0] (* :TAU) cos)]
     :abs [1 0 (=-> [stack 0] abs)]
     :sqrt [1 0
            (= [stack 0]
               (* (sign [stack 0])
                  (sqrt (abs [stack 0]))))]
     :if [3 -2
          (= [stack -2]
             (if (>= [stack 0] 0)
               [stack -1]
               [stack -2]))]
     :dup [1 1 (= [stack 1] [stack 0])]
     :swap2 [2 0
             (=float x [stack 0])
             (= [stack 0] [stack -1])
             (= [stack -1] x)]
     :swap3 [3 0
             (=float x [stack 0])
             (= [stack 0] [stack -2])
             (= [stack -2] x)]
     :swap4 [4 0
             (=float x [stack 0])
             (= [stack 0] [stack -3])
             (= [stack -3] x)]}))

(defn applied-size [prior-size [minimum-size size-change]]
  (when (>= prior-size minimum-size) (+ prior-size size-change)))

(defn count-stack-size [arg-count program]
  (reduce (fn [stack-sizes op]
            (conj stack-sizes
                  (let [last-size (peek stack-sizes)]
                    (if (number? op)
                      (inc last-size)
                      (or (applied-size last-size (environment op))
                          last-size)))))
          [arg-count]
          program))

(defn preprocess [in-count out-count program]
  (when-let [[final-stack-size final-program]
             (reduce (fn [[stack-size processed-program] op]
                       (when processed-program
                         (if (number? op)
                           [(inc stack-size) (conj processed-program op)]
                           (if-let [new-stack-size (applied-size stack-size
                                                                 (environment op))]
                             [new-stack-size
                              (conj processed-program op)]
                             [stack-size processed-program]))))
                     [in-count []]
                     program)]
    (when (>= final-stack-size out-count)
      final-program)))

(defn compile-program [in-count out-count program
                       & [{:keys [fn-name]
                           :or {fn-name 'evolved-fn}}]]
  (u/unquotable
   (let [stack-sizes (count-stack-size in-count program)
         max-stack-size (apply max stack-sizes)
         ending-stack-size (peek stack-sizes)]
     {:functions
      {fn-name
       (concat
        '([float ~(str out-count)]
          [input-args [float ~(str in-count)]]
          (= [float ~(str max-stack-size)]
             stack
             ~(vec (cons 'float
                         (take max-stack-size
                               (concat (map (fn [i]
                                              ['input-args (str i)])
                                            (range in-count))
                                       (repeat 0)))))))
        (map (fn [op stack-size]
               (if (number? op)
                 '(= [stack ~(str stack-size)] ~op)
                 (concat (list :block
                               (str "// " op))
                         (prewalk (fn [form]
                                    (if (and (vector? form)
                                             (= (first form) 'stack))
                                      (update form
                                              1
                                              (comp str
                                                    (partial + (dec stack-size))))
                                      form))
                                  (drop 2 (environment op))))))
             program
             stack-sizes)
        (list (vec (cons 'float
                         (map (fn [i]
                                ['stack (str (- (dec ending-stack-size)
                                                i))])
                              (reverse (range out-count)))))))}})))

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
