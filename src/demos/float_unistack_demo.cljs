(ns demos.float-unistack-demo
  (:require [sprog.util :as u]
            [sprog.webgl.core
             :refer [start-sprog!]
             :refer-macros [with-context]]
            [sprog.webgl.shaders :refer [run-purefrag-shader!]]
            [sprog.webgl.textures :refer [create-tex]]
            [sprog.dom.canvas :refer [maximize-gl-canvas
                                      canvas-resolution]]
            [linevo.engines.float-unistack :refer [eval-program
                                                   get-op-generator]]))

(let [op-generator (get-op-generator)
      random-program (vec (repeatedly 15 op-generator))]
  (defn random-fn [x y]
    (take 3
          (concat (map u/sigmoid
                       (eval-program [x y]
                                     random-program))
                  (repeat 0.5)))))

(def frag-shader
  '{:version "300 es"
    :precision {float highp
                int highp
                usampler2D highp}
    :uniforms {size vec2
               tex sampler2D}
    :outputs {frag-color vec4}
    :main
    ((= frag-color
        (vec4 (.rgb (texture tex (/ gl_FragCoord.xy size))) 1)))})

(defn init-sprog! [gl]
  (with-context gl
    (maximize-gl-canvas {:square? true})
    (let [resolution (canvas-resolution)]
      (run-purefrag-shader!
       frag-shader
       resolution
       {"size" resolution
        "tex" (let [pasma-size 100]
                (create-tex
                 :f8
                 pasma-size
                 {:filter-mode :nearest
                  :data
                  (js/Uint8Array.
                   (mapcat (fn [coords]
                             (conj (mapv #(min 255 (* % 255))
                                         (apply random-fn coords))
                                   0))
                           (for [x (range pasma-size)
                                 y (range pasma-size)]
                             (mapv #(-> %
                                        (/ (dec pasma-size))
                                        (* 2)
                                        (- 1))
                                   [x y]))))}))}))))

(defn init []
  (js/window.addEventListener
   "load"
   (fn [_]
     (start-sprog!
      init-sprog!
      nil))))
