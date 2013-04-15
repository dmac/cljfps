(ns fps.controls
  (:require [fps.systems :as systems])
  (:import [org.lwjgl.input Keyboard Mouse]))

(def ^:private movement-speed 10) ; meters per second

(def ^:private mouse-sensitivity 0.1)

(defn- distance-delta [dt]
  (-> dt (/ 1000) (* movement-speed)))

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

(defn handle-discrete-keyboard-input [game key-buffer]
  (reduce
    (fn [game k]
      (cond
        (and (= k Keyboard/KEY_SPACE)
             (not (get-in game [:entities :player :flight :airborn])))
          (assoc-in game [:entities :player :velocity :vy] 10)
        :else game))
    game
    key-buffer))

(defn handle-continuous-keyboard-input [game dt]
  (let [collidables (vals (dissoc (:entities game) :player))]
    (reduce
      (fn [game [k f]] (if (Keyboard/isKeyDown k) (f game) game))
      game
      [[Keyboard/KEY_A #(update-in % [:entities :player] systems/move :left (distance-delta dt) collidables)]
       [Keyboard/KEY_D #(update-in % [:entities :player] systems/move :right (distance-delta dt) collidables)]
       [Keyboard/KEY_S #(update-in % [:entities :player] systems/move :backward (distance-delta dt) collidables)]
       [Keyboard/KEY_W #(update-in % [:entities :player] systems/move :forward (distance-delta dt) collidables)]])))

(defn handle-keyboard-input [game dt]
  (-> game
      (handle-discrete-keyboard-input (get-key-buffer))
      (handle-continuous-keyboard-input dt)))

(defn handle-mouse-input [game]
  (let [dx (Mouse/getDX)
        dy (- (Mouse/getDY))]
    (-> game
        (update-in [:entities :player :orient :pitch] #(mod (+ % (* dy mouse-sensitivity)) 360))
        (update-in [:entities :player :orient :yaw] #(mod (+ % (* dx mouse-sensitivity)) 360)))))
