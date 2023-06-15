(ns linevo.demos.warp-iglu-demo
  (:require [sprog.util :as u]
            [sprog.webgl.core
             :refer [start-sprog!
                     update-sprog-state!]
             :refer-macros [with-context]]
            [sprog.iglu.core :refer [iglu->glsl]]
            [sprog.iglu.chunks.misc :refer [pos-chunk]]
            [sprog.webgl.shaders :refer [run-purefrag-shader!]]
            [sprog.dom.canvas :refer [maximize-gl-canvas
                                      canvas-resolution]]
            [linevo.engines.warp-iglu :refer [respecify-program
                                              generate-program
                                              compile-program]]
            [sprog.input.keyboard :refer [add-key-callback]]))

(defn program->glsl [program]
  (iglu->glsl
   pos-chunk
   (compile-program program
                    {:dimensions 3})
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
     ((=vec3 warpedPos
             (-> (vec3 (getPos) time)
                 uni->bi
                 twist))
      (= frag-color
         (vec4 (-> warpedPos
                   (distance (vec3 0))
                   (* :TAU 5)
                   sin
                   bi->uni
                   (pow 2.2)
                   vec3)
               1)))}))

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
  (let [program (generate-program
                 {:dimensions 3
                  :op-generator-options
                  {:amplitude-modifier (partial * 0.1)}})]
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
              (let [new-program (respecify-program program
                                                   {:dimensions 3})]
                {:program new-program
                 :frag-glsl (program->glsl new-program)}))))))
