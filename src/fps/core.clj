(ns fps.core
  (:require [fps.graphics :as graphics])
  (:use fps.component-entity
        [fps.levels :only [load-level]]
        [fps.util :only [find-first float-buffer]])
  (:import [org.lwjgl Sys]
           [org.lwjgl.opengl Display DisplayMode]
           [org.lwjgl.input Keyboard Mouse])
  (:gen-class))

(def movement-speed 10) ; meters per second
(def mouse-sensitivity 0.1)

(defn- get-time []
  (-> (Sys/getTime) (/ (Sys/getTimerResolution)) (* 1000) double))

(defn- distance-delta [dt]
  (-> dt (/ 1000) (* movement-speed)))

(defn- collides? [{{x1 :x y1 :y z1 :z} :position {w1 :width h1 :height d1 :depth} :volume :as entity1}
                  {{x2 :x y2 :y z2 :z} :position {w2 :width h2 :height d2 :depth} :volume :as entity2}]
  {:pre [(and (:position entity1) (:volume entity1))
         (and (:position entity2) (:volume entity2))]}
  (and (<= (Math/abs (- x1 x2)) (+ (/ w1 2) (/ w2 2)))
       (<= (Math/abs (- y1 y2)) (+ (/ h1 2) (/ h2 2)))
       (<= (Math/abs (- z1 z2)) (+ (/ d1 2) (/ d2 2)))))

(defn- render-all [{:keys [entities]}]
  (doseq [entity (filter :render (vals entities))]
    ((get-in entity [:render :fn]) entity)))

(defn- move [entity direction distance collidables]
  {:pre [(and (:position entity) (:orient entity))
         (contains? #{:forward :backward :left :right} direction)]}
  (let [yaw (get-in entity [:orient :yaw])
        update-x-amount (case direction
                          (:forward :backward) (* distance (Math/sin (Math/toRadians yaw)))
                          (:left :right) (* distance (Math/sin (Math/toRadians (- yaw 90)))))
        update-x-fn (case direction
                      (:forward :left) +
                      (:backward :right) -)
        update-z-amount (case direction
                          (:forward :backward) (* distance (Math/cos (Math/toRadians yaw)))
                          (:left :right) (* distance (Math/cos (Math/toRadians (- yaw 90)))))
        update-z-fn (case direction
                      (:forward :left) -
                      (:backward :right) +)
        new-entity-x (update-in entity [:position :x] update-x-fn update-x-amount)
        new-entity-z (update-in entity [:position :z] update-z-fn update-z-amount)
        new-entity-xz (assoc-in new-entity-x [:position :z] (get-in new-entity-z [:position :z]))]
    (or (find-first #(not-any? (partial collides? %) collidables)
                    [new-entity-xz new-entity-x new-entity-z])
        entity)))

(defn- move-vertical [entity distance collidables]
  {:pre [(and (:position entity) (:velocity entity))]}
  (let [new-entity (update-in entity [:position :y] + distance)]
    (if-let [collided-entity (find-first (partial collides? new-entity) collidables)]
      ; If a collision happens, entity's y value is set the boundary of the collided entity.
      ; This code should probably be decomposed and shared with the other move function.
      (let [collided-y (get-in collided-entity [:position :y])
            collided-hh (/ (get-in collided-entity [:volume :height]) 2)
            entity-hh (/ (get-in entity [:volume :height]) 2)
            resting-y (if (< 0 distance)
                        (- collided-y collided-hh entity-hh)
                        (+ collided-y collided-hh entity-hh))]
        (-> entity
            (assoc-in [:velocity :vy] 0)
            (assoc-in [:position :y] (Math/nextAfter (double resting-y) Double/MAX_VALUE))
            (assoc-in [:flight :airborn] false)))
      (assoc-in new-entity [:flight :airborn] true))))

(defn- handle-mouse-input [game]
  (let [dx (Mouse/getDX)
        dy (- (Mouse/getDY))]
    (-> game
        (update-in [:entities :player :orient :pitch] #(mod (+ % (* dy mouse-sensitivity)) 360))
        (update-in [:entities :player :orient :yaw] #(mod (+ % (* dx mouse-sensitivity)) 360)))))

(defn- get-key-buffer []
  (remove nil? (loop [k (Keyboard/next)
                      buffer []]
                 (if-not k
                   buffer
                   (if (Keyboard/getEventKeyState)
                     (recur (Keyboard/next)
                            (conj buffer (Keyboard/getEventKey)))
                     (recur (Keyboard/next)
                            buffer))))))

(defn- handle-keyboard-input [game key-buffer]
  (reduce
    (fn [game k]
      (cond
        (and (= k Keyboard/KEY_SPACE)
             (not (get-in game [:entities :player :flight :airborn])))
          (assoc-in game [:entities :player :velocity :vy] 10)
        :else game))
    game
    key-buffer))

(defn- handle-constant-keyboard-input [game dt]
  (let [collidables (vals (dissoc (:entities game) :player))]
    (reduce
      (fn [game [k f]] (if (Keyboard/isKeyDown k) (f game) game))
      game
      [[Keyboard/KEY_A #(update-in % [:entities :player] move :left (distance-delta dt) collidables)]
       [Keyboard/KEY_D #(update-in % [:entities :player] move :right (distance-delta dt) collidables)]
       [Keyboard/KEY_S #(update-in % [:entities :player] move :backward (distance-delta dt) collidables)]
       [Keyboard/KEY_W #(update-in % [:entities :player] move :forward (distance-delta dt) collidables)]])))

(defn- update-flight [game dt]
  (reduce
    (fn [game entity-index]
      (let [acceleration -20
            dt-seconds (/ dt 1000)
            y-delta (+ (* dt-seconds (get-in game (concat entity-index [:velocity :vy])))
                       (* acceleration 0.5 dt-seconds dt-seconds))
            vy-delta (* acceleration dt-seconds)
            collidables (vals (dissoc (:entities game) (last entity-index)))]
        (-> game
            (update-in (concat entity-index [:velocity :vy]) + vy-delta)
            (update-in entity-index move-vertical y-delta collidables))))
    game
    (for [entity-id (keys (:entities game))
          :let [entity (get-in game [:entities entity-id])]
          :when (and (:flight entity) (:velocity entity))]
      [:entities entity-id])))

(defn- update-all [game dt]
  (update-flight game dt))

(defn run []
  (graphics/init-window 1024 768)
  (graphics/init-gl)
  (loop [last-time (get-time)
         game {:entities (into {} (map (fn [entity] [(:id entity) entity])
                                       (concat
                                         [(entity :player
                                            (position :x 5 :y 2 :z 10)
                                            (volume :width 0.5 :height 1.9 :depth 0.5)
                                            (orient :pitch 0 :yaw 0)
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
                           handle-mouse-input
                           (handle-keyboard-input (get-key-buffer))
                           (handle-constant-keyboard-input dt)
                           (update-all dt))]
          (graphics/clear-screen)
          (graphics/look-through (get-in game [:entities :player]))
          (render-all new-game)
          (Display/update)
          (Display/sync 60)
          (recur new-time new-game))))))

(defn -main []
  (run))
