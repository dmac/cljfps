(ns fps.textures
  (:require [clojure.string :as string])
  (:use [clojure.java.io :only [resource]])
  (:import java.io.File
           [org.newdawn.slick.opengl TextureLoader]
           [org.newdawn.slick.util ResourceLoader]))

(def ^:private texture-map (atom {}))

(defn- load-texture [filename]
  (TextureLoader/getTexture "PNG" (ResourceLoader/getResourceAsStream filename)))

(defn load-textures []
  (let [texture-files (->> (File. (.getFile (resource "textures"))) .listFiles (map #(.getName %)))]
    (doseq [texture-file texture-files]
      (let [texture-key (-> texture-file (string/split #"\.") first keyword)
            texture (load-texture (str "textures/" texture-file))]
        (swap! texture-map assoc texture-key texture)))))

(defn get-texture [texture-key]
  (texture-key @texture-map))
