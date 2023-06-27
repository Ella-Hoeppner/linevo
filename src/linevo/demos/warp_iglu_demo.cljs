(ns linevo.demos.warp-iglu-demo
  (:require [sprog.util :as u]
            [sprog.webgl.core
             :refer [start-sprog!
                     update-sprog-state!]
             :refer-macros [with-context]]
            [sprog.iglu.core :refer [iglu->glsl]]
            [sprog.iglu.chunks.misc :refer [pos-chunk
                                            sigmoid-chunk]]
            [sprog.iglu.chunks.color :refer [hsl-to-rgb-chunk]]
            [sprog.webgl.shaders :refer [run-purefrag-shader!]]
            [sprog.dom.canvas :refer [maximize-gl-canvas
                                      canvas-resolution]]
            [linevo.engines.warp-iglu :refer [respecify-program
                                              generate-program
                                              compile-program]]
            [sprog.input.keyboard :refer [add-key-callback]]))

(def program-options
  {:dimensions 3
   :op-generator-options
   {:amplitude-modifier (partial * 0.1)}})

(defn program->glsl [program]
  (iglu->glsl
   pos-chunk
   sigmoid-chunk
   hsl-to-rgb-chunk
   (compile-program program program-options)
   {:constants {:TAU u/TAU}}
   '{:version "300 es"
     :precision {float highp
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

(defn init-sprog! [gl]
  (let [program (generate-program program-options)]
    {:program program
     :frag-glsl (program->glsl program)}))

(defn init []
  (js/window.addEventListener
   "load"
   (fn [_]
     (start-sprog!
      init-sprog!
      update-sprog!)))
  (add-key-callback
   " "
   #(update-sprog-state!
     (fn [{:keys [program] :as state}]
       (merge state
              (let [new-program (respecify-program program program-options)]
                {:program new-program
                 :frag-glsl (program->glsl new-program)}))))))
