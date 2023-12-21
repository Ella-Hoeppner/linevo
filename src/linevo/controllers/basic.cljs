(ns linevo.controllers.basic
  (:require [linevo.rand :refer [rand-fn-rand-int]]
            [linevo.gp :refer [breed]]))

(defn create-controller! [op-generator
                          & [{:keys [rand-fn
                                     scratch-min-length
                                     scratch-max-length
                                     breed-min-length
                                     breed-max-length
                                     initial-population-size
                                     initial-population
                                     preprocessor]
                              :or {rand-fn rand
                                   scratch-min-length 5
                                   scratch-max-length 15
                                   breed-min-length 1
                                   breed-max-length 40
                                   initial-population-size 1
                                   preprocessor identity}}]]
  (let [scratch-program (fn []
                          (some preprocessor
                                (repeatedly
                                 #(vec (repeatedly
                                        (rand-fn-rand-int
                                         rand-fn
                                         scratch-min-length
                                         (inc scratch-max-length))
                                        op-generator)))))
        population-atom (atom (vec (if initial-population
                                     initial-population
                                     (repeatedly initial-population-size
                                                 scratch-program))))
        deletions-atom (atom ())
        index-atom (atom 0)
        breed-program (fn []
                        (some (comp preprocessor
                                    #(when (<= breed-min-length
                                               (count %)
                                               breed-max-length)
                                       %))
                              (repeatedly #(breed op-generator
                                                  @population-atom))))
        rectify-index! (fn []
                         (swap! index-atom
                                #(-> %
                                     (max 0)
                                     (min (dec (count @population-atom))))))
        move-index! (fn [movement]
                      (swap! index-atom (partial + movement))
                      (rectify-index!))]
    {:scratch (fn []
                (reset! index-atom (count @population-atom))
                (swap! population-atom #(conj % (scratch-program)))
                (last @population-atom))
     :breed (fn []
              (reset! index-atom (count @population-atom))
              (swap! population-atom
                     #(conj % (breed-program)))
              (last @population-atom))
     :delete (fn [& [population-index]]
               (when (> (count @population-atom) 1)
                 (let [index (or population-index @index-atom)]
                   (swap! deletions-atom
                          #(conj % (nth @population-atom index)))
                   (swap! population-atom
                          #(vec (concat (take index %)
                                        (drop (inc index) %))))
                   (rectify-index!)))
               (@population-atom @index-atom))
     :undelete (fn []
                 (when (pos? (count @deletions-atom))
                   (reset! index-atom (count @population-atom))
                   (swap! population-atom #(conj % (first @deletions-atom)))
                   (swap! deletions-atom rest))
                 (@population-atom @index-atom))
     :move (fn [movement]
             (move-index! movement)
             (@population-atom @index-atom))
     :set-index (fn [index]
                  (reset! index-atom index)
                  (rectify-index!)
                  (@population-atom @index-atom))
     :get-program (fn [& [population-index]]
                    (let [index (or population-index @index-atom)]
                      (@population-atom index)))
     :get-population (fn [] @population-atom)}))
