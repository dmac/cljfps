(ns fps.textures
  (:require [clojure.string :as string])
  (:use [clojure.java.io :only [resource]])
  (:import java.io.File
           [org.newdawn.slick.opengl TextureLoader]
           [org.newdawn.slick.util ResourceLoader]))

(def ^:private texture-id-map (atom {}))

(defn- load-texture [filename]
  (TextureLoader/getTexture "PNG" (ResourceLoader/getResourceAsStream filename)))

(defn load-textures []
  (let [texture-files (->> (File. (.getFile (resource "textures"))) .listFiles (map #(.getName %)))]
    (doseq [texture-file texture-files]
      (let [texture-key (-> texture-file (string/split #"\.") first keyword)
            texture (load-texture (str "textures/" texture-file))]
        (swap! texture-id-map assoc texture-key (.getTextureID texture))))))

(defn get-texture-id [texture-key]
  (texture-key @texture-id-map))

(defn- texture-coords-from-atlas-coords [[x y] texture-width texture-height atlas-width atlas-height]
  [[(/ (+ x 0.5) atlas-width)                   (/ (+ y 0.5) atlas-height)]
   [(/ (- (+ x texture-width) 0.5) atlas-width) (/ (+ y 0.5) atlas-height)]
   [(/ (- (+ x texture-width) 0.5) atlas-width) (/ (- (+ y texture-height) 0.5) atlas-height)]
   [(/ (+ x 0.5) atlas-width)                   (/ (- (+ y texture-height) 0.5) atlas-height)]])

(defn texture-coords [texture-key]
  (texture-key (into {}
                     (for [[texture-key atlas-coords]
                           {:crate [0 0]
                            :stone [256 0]}]
                       [texture-key (texture-coords-from-atlas-coords atlas-coords 256 256 512 512)]))))
