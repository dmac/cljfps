(ns fps.core
  (:require [fps.graphics :as graphics]
            [fps.controls :as controls]
            [fps.systems :as systems])
  (:use fps.component-entity
        [fps.levels :only [load-level]])
  (:import [org.lwjgl Sys]
           [org.lwjgl.opengl Display])
  (:gen-class))

(defn- get-time []
  (-> (Sys/getTime) (/ (Sys/getTimerResolution)) (* 1000) double))

(defn- render-all [{:keys [entities]}]
  (doseq [entity (filter :render (vals entities))]
    ((get-in entity [:render :fn]) entity)))

(defn- update-all [game dt]
  (systems/update-flight game dt))

(defn run []
  (graphics/init-window)
  (graphics/init-gl)
  (loop [last-time (get-time)
         game {:entities (into {} (map (fn [entity] [(:id entity) entity])
                                       (concat
                                         [(entity :player
                                            (position :x 0 :y 2 :z 0)
                                            (volume :width 0.5 :height 1.9 :depth 0.5)
                                            (orient :pitch 0 :yaw 180)
                                            (velocity :vy 0)
                                            (flight :airborn false))
                                          (entity :floor
                                            (position :x 0 :y 0 :z 0)
                                            (volume :width 100 :height 0 :depth 100))]
                                         (load-level "room.dat"))))}]
    (if (Display/isCloseRequested)
      (System/exit 0)
      ;(Display/destroy)
      (do
        (let [new-time (get-time)
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
          (recur new-time new-game))))))

(defn -main []
  (run))
