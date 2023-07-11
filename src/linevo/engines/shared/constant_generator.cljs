(ns linevo.engines.shared.constant-generator
  (:require [linevo.rand :refer [rand-fn-geometric
                                 rand-fn-exponential]]))

(defn get-numeric-constant-generator [& [{:keys [rand-fn
                                         int-chance
                                         int-p
                                         float-lambda]
                                  :or {rand-fn rand
                                       int-chance 0.5
                                       int-p 0.5
                                       float-lambda 0.5}}]]
  (fn []
    (* (if (< (rand-fn) 0.5) -1 1)
       (if (< (rand-fn) int-chance)
         (if (< (rand-fn) int-p)
           0
           (inc (rand-fn-geometric rand-fn int-p)))
         (rand-fn-exponential rand-fn float-lambda)))))