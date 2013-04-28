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

(defn- texture-coords-from-atlas-coords [[atlas-x atlas-y] atlas-width atlas-height]
  ; Buffering each texture by a small amount prevents the border of one texture leaking onto another.
  (let [buffer 0.001
        step-x (/ 1 atlas-width)
        step-y (/ 1 atlas-height)
        buffered-step-x (- step-x (* buffer 2))
        buffered-step-y (- step-y (* buffer 2))
        top-left-x (+ (* atlas-x step-x) buffer)
        top-left-y (+ (* atlas-y step-y) buffer)]
    [[top-left-x top-left-y]
     [(+ top-left-x buffered-step-x) top-left-y]
     [(+ top-left-x buffered-step-x) (+ top-left-y buffered-step-y)]
     [top-left-x (+ top-left-y buffered-step-y)]]))

(defn texture-coords [texture-key]
  (texture-key (into {}
                     (for [[texture-key atlas-coords]
                           {:crate [0 0]
                            :stone [1 0]}]
                       [texture-key (texture-coords-from-atlas-coords atlas-coords 2 2)]))))
