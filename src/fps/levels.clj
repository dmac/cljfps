(ns fps.levels
  (:require [clojure.string :as string]
            [fps.graphics :as graphics])
  (:use [clojure.java.io :only [resource]]
        [fps.component-entity]
        [fps.utils :only [indexed]]))

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
                           slices-with-metadata))]
    (filter (comp not nil?)
            (for [[y-index slice] (indexed slices)
                  [z-index row] (indexed (string/split slice #"\n"))
                  [x-index c] (indexed (seq row))
                  :let [texture-type (case c
                                       \S :stone
                                       \C :crate
                                       nil)
                        block-size 1
                        hbs (/ block-size 2.0)]]
              (when texture-type
                (entity (next-id)
                  (position :x (+ x-index hbs) :y (+ y-index hbs) :z (+ z-index hbs))
                  (volume :width block-size :height block-size :depth block-size)
                  (texture :texture texture-type)
                  (render :fn graphics/draw-box)))))))
