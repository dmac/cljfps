(ns fps.levels
  (:require [clojure.string :as string]
            [fps.graphics :as graphics])
  (:use [clojure.java.io :only [resource]]
        [fps.component-entity]
        [fps.utils :only [indexed vassoc-in]]))

(def ^:private current-id (atom 0))

(defn- next-id []
  (swap! current-id inc))

(defn load-level [filename]
  (let [level-string (slurp (resource (str "levels/" filename)))
        slices-with-metadata (-> level-string
                                 (string/split #"\n\n")
                                 (#(map string/trim %)))
        slices (apply concat
                      (map (fn [slice-with-metadata]
                             (let [[metadata slice] (string/split slice-with-metadata #"\n" 2)

                                   num-repeats (Integer/parseInt metadata)]
                               (repeat num-repeats slice)))
                           slices-with-metadata))
        box-init-fn (fn [box]
                      (-> box
                          (assoc-in [:render :vertex-buffer-id] (graphics/init-vertex-buffer))
                          graphics/regenerate-box-vertex-data!))]
    (reduce (fn [world [x y z box]]
              (vassoc-in world [x y z] box))
            []
            (for [[y slice] (indexed slices)
                  [z row] (indexed (string/split slice #"\n"))
                  [x c] (indexed (seq row))
                  :let [material-type (case c
                                        \S :stone
                                        \C :crate
                                        nil)
                        block-size 1
                        hbs (/ block-size 2.0)]]
              [x y z
               (when material-type
                 (entity (next-id)
                   (position :x (+ x hbs) :y (+ y hbs) :z (+ z hbs))
                   (volume :width block-size :height block-size :depth block-size)
                   (material :material material-type)
                   (render :fn graphics/draw-box
                           :init-fn box-init-fn)))]))))
