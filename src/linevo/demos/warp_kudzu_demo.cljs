(ns linevo.demos.warp-kudzu-demo
  (:require [hollow.util :as u]
            [hollow.webgl.core
             :refer [start-hollow!
                     hollow-state
                     merge-hollow-state!]
             :refer-macros [with-context]]
            [kudzu.core :refer [kudzu->glsl]]
            [kudzu.chunks.misc :refer [sigmoid-chunk]]
            [kudzu.chunks.color.hsl :refer [hsl->rgb-chunk]]
            [hollow.webgl.shaders :refer [run-purefrag-shader!]]
            [hollow.dom.canvas :refer [maximize-gl-canvas
                                       canvas-resolution]]
            [linevo.engines.warp-kudzu :refer [respecify-program
                                               get-op-generator
                                               compile-program
                                               preprocess]]
            [linevo.controllers.basic :refer [create-controller!]]
            [hollow.input.keyboard :refer [add-left-right-key-callback
                                           add-key-callback]]))

(def program-options
  {:dimensions 3})

(def op-generator
  (get-op-generator (merge program-options
                           {:amplitude-modifier (partial * 0.1)})))

(defn program->glsl [program]
  (kudzu->glsl
   sigmoid-chunk
   hsl->rgb-chunk
   (compile-program program program-options)
   {:constants {:TAU u/TAU}}
   '{:precision {float highp
                 int highp
                 usampler2D highp}
     :uniforms {resolution vec2
                tex sampler2D
                time float}
     :outputs {frag-color vec4}
     :main
     ((=vec3 pos (vec3 (pixel-pos) time))
      (=vec3 noise (-> (twist pos)
                       (- pos)
                       (* 10)))
      (= frag-color
         (-> (vec3 (sigmoid noise.x)
                   (* 0.5 (sigmoid noise.y))
                   (sigmoid noise.z))
             hsl->rgb
             (pow (vec3 2.2))
             (vec4 1))))}))

(defn update-hollow! [{:keys [gl frag-glsl] :as state}]
  (with-context gl
    (maximize-gl-canvas {:square? true})
    (let [resolution (canvas-resolution)]
      (run-purefrag-shader!
       frag-glsl
       resolution
       {:resolution resolution
        :time (* 0.01 (u/seconds-since-startup))})))
  state)

(defn display-program! [program]
  (merge-hollow-state!
   {:program program
    :frag-glsl (program->glsl program)}))

(defn init-hollow! [gl]
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
     (start-hollow!
      init-hollow!
      update-hollow!)))
  (add-key-callback
   " "
   #(display-program! (respecify-program (:program (hollow-state))
                                         program-options))))
