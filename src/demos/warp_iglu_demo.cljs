(ns demos.warp-iglu-demo
  (:require [sprog.util :as u]
            [sprog.webgl.core
             :refer [start-sprog!]
             :refer-macros [with-context]]
            [sprog.iglu.core :refer [iglu->glsl]]
            [sprog.iglu.chunks.misc :refer [pos-chunk]]
            [sprog.webgl.shaders :refer [run-purefrag-shader!]]
            [sprog.dom.canvas :refer [maximize-gl-canvas
                                      canvas-resolution]]
            [linevo.engines.warp-iglu :refer [generate-program
                                              compile-program]]))

(def frag-shader
  (iglu->glsl
   pos-chunk
   (compile-program (let [program (generate-program {:dimensions 3})]
                      (u/log (map :op program))
                      program)
                    {:dimensions 3})
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
         (vec4 (if (-> warpedPos
                       (distance (vec3 0))
                       (mod 0.1)
                       (< 0.01))
                 (vec3 1)
                 (vec3 0))
               1)))}))

(defn update-sprog! [{:keys [gl]}]
  (with-context gl
    (maximize-gl-canvas {:square? true})
    (let [resolution (canvas-resolution)]
      (run-purefrag-shader!
       frag-shader
       resolution
       {"size" resolution
        "time" (* 0.01 (u/seconds-since-startup))}))))

(defn init []
  (js/window.addEventListener
   "load"
   (fn [_]
     (start-sprog!
      nil
      update-sprog!))))
