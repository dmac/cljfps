(ns fps.core
  (:require [fps.graphics :as graphics]
            [fps.controls :as controls]
            [fps.systems :as systems])
  (:use fps.component-entity
        [fps.levels :only [load-level]])
  (:import [org.lwjgl Sys]
           [org.lwjgl.opengl Display])
  (:gen-class))

; TODO
; - billboard enemies and items
;   - as a proxy, maybe two perpendicular quads with same texture?

(defn- get-time []
  (-> (Sys/getTime) (/ (Sys/getTimerResolution)) (* 1000) double))

(def ^:private fps (atom 0))
(def ^:private last-fps (atom (get-time)))

(defn- update-fps []
  (swap! fps inc)
  (let [new-time (get-time)]
    (when (> (- new-time @last-fps) 1000)
      (Display/setTitle (str "FPS: " @fps))
      (reset! fps 0)
      (reset! last-fps new-time))))

(defn- render-all [{:keys [entities world] :as game}]
  (doseq [entity (filter :render (conj (vals entities) world))]
    ((get-in entity [:render :fn]) entity)))

(defn- update-all [game dt]
  (systems/update-flight game dt))

(defn run []
  (graphics/init-window)
  (graphics/init-gl)
  (let [world (load-level "flat.dat")]
    (graphics/regenerate-world-vertex-data! world)
    (loop [last-time (get-time)
           game {:entities (into {} (map (fn [entity] [(:id entity) entity])
                                         [(entity :player
                                            (position :x 0 :y 5 :z 0)
                                            (volume :width 0.5 :height 1.9 :depth 0.5)
                                            (orient :pitch 0 :yaw 180)
                                            (velocity :vy 0)
                                            (flight :airborn false))
                                          (entity :floor
                                            (position :x 0 :y 0 :z 0)
                                            (volume :width 100 :height 0 :depth 100))]))
                 :world world}]
      (if (Display/isCloseRequested)
        (System/exit 0)
        ;(Display/destroy)
        (do
          (let [new-time (get-time)
                ; TODO: Make :dt a property of game
                dt (- new-time last-time)
                new-game (-> game
                             controls/handle-mouse-input
                             (controls/handle-keyboard-input dt)
                             (update-all dt))]
            (graphics/clear-screen)
            (graphics/look-through (get-in game [:entities :player]))
            (render-all new-game)
            (Display/update)
            (Display/sync 60)
            (update-fps)
            (recur new-time new-game)))))))

(defn -main []
  (run))
