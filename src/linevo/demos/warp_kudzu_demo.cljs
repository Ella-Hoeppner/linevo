(ns linevo.demos.warp-kudzu-demo
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
            [linevo.engines.warp-kudzu :refer [respecify-program
                                               get-op-generator
                                               compile-program
                                               preprocess]]
            [linevo.controllers.basic :refer [create-controller!]]
            [sprog.input.keyboard :refer [add-left-right-key-callback
                                          add-key-callback]]))

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
   (compile-program program program-options)
   {:constants {:TAU u/TAU}}
   '{:precision {float highp
                 int highp
                 usampler2D highp}
     :uniforms {size vec2
                tex sampler2D
                time float}
     :outputs {frag-color vec4}
     :main
     ((=vec3 pos (vec3 (uni->bi (getPos)) time))
      (=vec3 noise (-> (twist pos)
                       (- pos)
                       (* 10)))
      (= frag-color
         (-> (vec3 (sigmoid noise.x)
                   (* 0.5 (sigmoid noise.y))
                   (sigmoid noise.z))
             hsl2rgb
             (pow (vec3 2.2))
             (vec4 1))))}))

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
    :frag-glsl (program->glsl program)}))

(defn init-sprog! [gl]
  (let [{:keys [move
                get-program]
         :as controller}
        (create-controller! op-generator
                            {:scratch-min-length 10
                             :scratch-max-length 80
                             :preprocessor preprocess})
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
       :frag-glsl (program->glsl program)})))

(defn init []
  (js/window.addEventListener
   "load"
   (fn [_]
     (start-sprog!
      init-sprog!
      update-sprog!)))
  (add-key-callback
   " "
   #(display-program! (respecify-program (:program (sprog-state))
                                         program-options))))
