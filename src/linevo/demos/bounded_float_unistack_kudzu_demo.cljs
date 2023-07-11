(ns linevo.demos.bounded-float-unistack-kudzu-demo
  (:require [sprog.util :as u]
            [sprog.webgl.core
             :refer [start-sprog!
                     sprog-state
                     merge-sprog-state!]
             :refer-macros [with-context]]
            [sprog.kudzu.core :refer [kudzu->glsl]]
            [sprog.kudzu.chunks.misc :refer [pos-chunk
                                             sigmoid-chunk]]
            [sprog.kudzu.chunks.color :refer [hsl-to-rgb-chunk]]
            [sprog.webgl.shaders :refer [run-purefrag-shader!]]
            [sprog.dom.canvas :refer [maximize-gl-canvas
                                      canvas-resolution]]
            [linevo.engines.bounded-float-unistack-kudzu
             :refer [get-op-generator
                     compile-program
                     preprocess]]
            [linevo.controllers.basic :refer [create-controller!]]
            [sprog.input.keyboard :refer [add-left-right-key-callback
                                          add-key-callback]]))

(def evolved-fn-in-count 2)
(def evolved-fn-out-count 3)

(def program-options
  {:dimensions 3})

(def op-generator
  (get-op-generator (merge program-options
                           {:amplitude-modifier (partial * 0.1)})))

(defn program->glsl [program]
  (kudzu->glsl
   pos-chunk
   sigmoid-chunk
   hsl-to-rgb-chunk
   (compile-program evolved-fn-in-count 
                    evolved-fn-out-count
                    program program-options)
   {:constants {:TAU u/TAU}}
   (u/unquotable
    '{:precision {float highp
                  int highp
                  usampler2D highp}
      :uniforms {size vec2
                 tex sampler2D
                 time float}
      :outputs {frag-color vec4}
      :main
      ((=vec3 pos (vec3 (uni->bi (getPos)) time))
       (= [float ~(str evolved-fn-out-count)]
          result 
          (evolved-fn [float pos.x pos.y]))
       (= frag-color
          (vec4 (if (|| (> [result "0"] 1)
                        (< [result "0"] -1))
                  1
                  0)
                (if (|| (> [result "1"] 1)
                        (< [result "1"] -1))
                  1
                  0)
                (if (|| (> [result "2"] 1)
                        (< [result "2"] -1))
                  1
                  0)
                1)))})))

(defn update-sprog! [{:keys [gl frag-glsl] :as state}]
  (with-context gl
    (maximize-gl-canvas {:square? true})
    (let [resolution (canvas-resolution)]
      (run-purefrag-shader!
       frag-glsl
       resolution
       {"size" resolution
        "time" (* 0.01 (u/seconds-since-startup))})))
  state)

(defn display-program! [program]
  (merge-sprog-state!
   {:program program
    :frag-glsl (u/log (program->glsl program))}))

(defn init-sprog! [gl]
  (let [{:keys [move
                get-program]
         :as controller}
        (create-controller!
         op-generator
         {:scratch-min-length 5
          :scratch-max-length 10
          :preprocessor (partial preprocess 
                                 evolved-fn-in-count 
                                 evolved-fn-out-count)})
        display! #(display-program! (get-program))]
    (doseq [[key-name fn-name]
            [["s" :scratch]
             ["b" :breed]
             ["x" :delete]
             ["z" :undelete]]]
      (add-key-callback key-name #(do ((fn-name controller)) (display!))))
    (add-left-right-key-callback #(do (move %) (display!)))
    (let [program (get-program)]
      {:program program
       :frag-glsl (u/log (program->glsl program))})))

(defn init []
  (js/window.addEventListener
   "load"
   (fn [_]
     (start-sprog!
      init-sprog!
      update-sprog!))))
