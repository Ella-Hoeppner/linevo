(ns linevo.demos.float-unistack-demo
  (:require [hollow.util :as u]
            [hollow.webgl.core
             :refer [start-hollow!
                     hollow-context
                     hollow-state]
             :refer-macros [with-context]]
            [hollow.webgl.shaders :refer [run-purefrag-shader!]]
            [hollow.webgl.textures :refer [create-tex
                                          set-tex-data!]]
            [hollow.dom.canvas :refer [maximize-gl-canvas
                                      canvas-resolution]]
            [linevo.engines.float-unistack :refer [eval-program
                                                   get-op-generator]]
            [linevo.controllers.basic :refer [create-controller!]]
            [hollow.input.keyboard :refer [add-key-callback
                                          add-left-right-key-callback]]))

(def pasma-resolution 50)

(def op-generator (get-op-generator))

(def frag-shader
  '{:precision {float highp
                int highp
                usampler2D highp}
    :uniforms {size vec2
               tex sampler2D}
    :outputs {frag-color vec4}
    :main
    ((= frag-color
        (vec4 (.rgb (texture tex (/ gl_FragCoord.xy size))) 1)))})

(defn get-program-data [program]
  (let [program-fn (fn [x y]
                     (take 3
                           (concat (map u/sigmoid
                                        (eval-program [x y]
                                                      program))
                                   (repeat 0.5))))]
    (js/Uint8Array.
     (mapcat (fn [coords]
               (conj (mapv #(min 255 (* % 255))
                           (apply program-fn coords))
                     0))
             (for [x (range pasma-resolution)
                   y (range pasma-resolution)]
               (mapv #(-> %
                          (/ (dec pasma-resolution))
                          (* 2)
                          (- 1))
                     [x y]))))))

(defn display-program-pasma! [program]
  (set-tex-data! (hollow-context)
                 (:tex (hollow-state))
                 (get-program-data program)))

(defn update-hollow! [{:keys [gl tex] :as state}]
  (with-context gl
    (maximize-gl-canvas {:square? true})
    (let [resolution (canvas-resolution)]
      (run-purefrag-shader!
       frag-shader
       resolution
       {"size" resolution
        "tex" tex}))
    state))

(defn init-hollow! [gl]
  (with-context gl
    (let [{:keys [move
                  get-program]
           :as controller}
          (create-controller! op-generator)
          display! #(display-program-pasma! (get-program))]
      (doseq [[key-name fn-name]
              [["s" :scratch]
               ["b" :breed]
               ["x" :delete]
               ["z" :undelete]]]
        (add-key-callback key-name #(do ((fn-name controller)) (display!))))
      (add-left-right-key-callback #(do (move %) (display!)))
      {:tex (create-tex :f8
                        pasma-resolution
                        {:filter-mode :nearest
                         :data (get-program-data (get-program))})})))

(defn init []
  (js/window.addEventListener
   "load"
   (fn [_]
     (start-hollow!
      init-hollow!
      update-hollow!))))
