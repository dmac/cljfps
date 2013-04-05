(ns fps.core
  (:import [org.lwjgl Sys]
           [org.lwjgl.opengl Display DisplayMode GL11]
           [org.lwjgl.util.glu GLU]
           [org.lwjgl.input Keyboard Mouse])
  (:gen-class))

(defrecord Game [camera cube])
(defrecord Camera [x y z pitch yaw])
(defrecord Cube [points color])

(def movement-speed 10) ; m/s
(def mouse-sensitivity 0.1)

(defn- select-indices [coll indices]
  (let [selected-map (select-keys coll indices)]
    (into [] (map #(get selected-map %) indices))))

(defn- get-time []
  (-> (Sys/getTime) (/ (Sys/getTimerResolution)) (* 1000) double))

(defn- distance-traveled [dt]
  (-> dt (/ 1000) (* movement-speed)))

(defn- set-color [[r g b]]
  (GL11/glColor3f r g b))

(defn- clear-screen []
  (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
  (GL11/glLoadIdentity))

(defn- draw [{:keys [camera cube]}]
  (set-color (:color cube))
  (GL11/glPushMatrix)
  (GL11/glBegin GL11/GL_QUADS)
  (doseq [[x y z] (select-indices (:points cube) [0 1 2 3])] (GL11/glVertex3f x y z))
  (set-color [0.5 1.0 0.5])
  (doseq [[x y z] (select-indices (:points cube) [0 1 5 4])] (GL11/glVertex3f x y z))
  (set-color [1.0 0.5 0.5])
  (doseq [[x y z] (select-indices (:points cube) [4 5 6 7])] (GL11/glVertex3f x y z))
  (set-color [0.5 0.5 0.5])
  (doseq [[x y z] (select-indices (:points cube) [2 3 7 6])] (GL11/glVertex3f x y z))
  (GL11/glEnd)
  (GL11/glPopMatrix))

(defn- look-through [{:keys [x y z pitch yaw] :as camera}]
  (GL11/glRotatef pitch 1 0 0)
  (GL11/glRotatef yaw 0 1 0)
  ;(GL11/glTranslatef x y z))
  (GL11/glTranslatef (- x) (- y) (- z)))

(defn- move-forward [camera distance]
  (-> camera
      (update-in [:x] + (* distance (Math/sin (Math/toRadians (:yaw camera)))))
      (update-in [:z] - (* distance (Math/cos (Math/toRadians (:yaw camera)))))))

(defn- move-backward [camera distance]
  (-> camera
      (update-in [:x] - (* distance (Math/sin (Math/toRadians (:yaw camera)))))
      (update-in [:z] + (* distance (Math/cos (Math/toRadians (:yaw camera)))))))

(defn- move-left [camera distance]
  (-> camera
      (update-in [:x] + (* distance (Math/sin (Math/toRadians (- (:yaw camera) 90)))))
      (update-in [:z] - (* distance (Math/cos (Math/toRadians (- (:yaw camera) 90)))))))

(defn- move-right [camera distance]
  (-> camera
      (update-in [:x] - (* distance (Math/sin (Math/toRadians (- (:yaw camera) 90)))))
      (update-in [:z] + (* distance (Math/cos (Math/toRadians (- (:yaw camera) 90)))))))

(defn- handle-mouse-input [game]
  (let [dx (Mouse/getDX)
        dy (- (Mouse/getDY))]
    (-> game
        (update-in [:camera :pitch] #(mod (+ % (* dy mouse-sensitivity)) 360))
        (update-in [:camera :yaw] #(mod (+ % (* dx mouse-sensitivity)) 360)))))

(defn- handle-keyboard-input [game dt]
  (reduce
    (fn [game [k f]] (if (Keyboard/isKeyDown k) (f game) game))
    game
    [[Keyboard/KEY_A #(update-in % [:camera] move-left (distance-traveled dt))]
     [Keyboard/KEY_D #(update-in % [:camera] move-right (distance-traveled dt))]
     [Keyboard/KEY_S #(update-in % [:camera] move-backward (distance-traveled dt))]
     [Keyboard/KEY_W #(update-in % [:camera] move-forward (distance-traveled dt))]]))

(defn- init-gl []
  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glLoadIdentity)
  (GLU/gluPerspective 70.0 (/ 800 600) 0.1 1000)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClearDepth 1)
  (GL11/glEnable GL11/GL_DEPTH_TEST)
  (GL11/glDepthFunc GL11/GL_LEQUAL)
  (GL11/glHint GL11/GL_PERSPECTIVE_CORRECTION_HINT GL11/GL_NICEST))

(defn- init-window [w h]
  (Display/setDisplayMode (DisplayMode. w h))
  (Display/setTitle "FPS")
  (Display/create)
  (Mouse/setGrabbed true))

(defn run []
  (init-window 800 600)
  (init-gl)
  (loop [last-time (get-time)
         game (->Game
                (->Camera 5 2 10 0 0)
                (->Cube [[0 0 0] [0 10 0] [10 10 0] [10 0 0]
                         [0 0 -10] [0 10 -10] [10 10 -10] [10 0 -10]]
                        [0.5 0.5 1.0]))]
    (if (Display/isCloseRequested)
      (Display/destroy)
      (do
        (let [new-time (get-time)
              dt (- new-time last-time)
              new-game (-> game handle-mouse-input (handle-keyboard-input dt))]
          (clear-screen)
          (look-through (:camera game))
          (draw new-game)
          (Display/update)
          (Display/sync 60)
          (recur new-time new-game))))))

(defn -main []
  (run))
