(ns fps.core
  (:import [java.nio ByteBuffer ByteOrder]
           [org.lwjgl Sys]
           [org.lwjgl.opengl Display DisplayMode GL11]
           [org.lwjgl.util.glu GLU]
           [org.lwjgl.input Keyboard Mouse]
           [org.newdawn.slick.opengl TextureLoader]
           [org.newdawn.slick.util ResourceLoader])
  (:gen-class))

(defrecord Game [camera entities])
(defrecord Camera [x y z pitch yaw])
(defrecord Box [x y z width height depth texture])

(def movement-speed 10) ; meters per second
(def mouse-sensitivity 0.1)

(def crate-texture (atom nil))

(defn- float-buffer [& args]
  (-> (doto (ByteBuffer/allocateDirect 16) (.order (ByteOrder/nativeOrder)))
      .clear .asFloatBuffer
      (.put (float-array args))
      .flip))

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

(defn- box-points [{:keys [x y z width height depth] :as box}]
  (let [w-2 (/ width 2) h-2 (/ height 2) d-2 (/ depth 2)]
    [[(- x w-2) (- y h-2) (+ z d-2)] [(- x w-2) (+ y h-2) (+ z d-2)]
     [(+ x w-2) (+ y h-2) (+ z d-2)] [(+ x w-2) (- y h-2) (+ z d-2)]
     [(- x w-2) (- y h-2) (- z d-2)] [(- x w-2) (+ y h-2) (- z d-2)]
     [(+ x w-2) (+ y h-2) (- z d-2)] [(+ x w-2) (- y h-2) (- z d-2)]]))

(defn- draw-box [{:keys [texture] :as box}]
  (set-color [1.0 1.0 1.0])
  (.bind texture)
  (GL11/glPushMatrix)
  (GL11/glBegin GL11/GL_QUADS)
  (let [points (box-points box)
        texture-points [[0 0] [0 1] [1 1] [1 0]]]
    (doseq [point-indices [[0 1 2 3] [4 5 1 0] [7 6 5 4] [3 2 6 7] [3 7 4 0] [1 5 6 2]]]
      (doseq [[[tx ty] [x y z]] (partition 2 (interleave texture-points
                                                         (select-indices points point-indices)))]
        (GL11/glTexCoord2f tx ty)
        (GL11/glVertex3f x y z))))
  (GL11/glEnd)
  (GL11/glPopMatrix))

(defn- draw [{:keys [camera entities]}]
  (doseq [entity entities]
    (draw-box entity)))

(defn- look-through [{:keys [x y z pitch yaw] :as camera}]
  (GL11/glRotatef pitch 1 0 0)
  (GL11/glRotatef yaw 0 1 0)
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

(defn- load-textures []
  (reset! crate-texture
          (TextureLoader/getTexture "PNG" (ResourceLoader/getResourceAsStream "textures/crate.png"))))

(defn- init-gl []
  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glLoadIdentity)
  (GLU/gluPerspective 70.0 (/ 800 600) 0.1 1000)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClearDepth 1)
  (GL11/glEnable GL11/GL_DEPTH_TEST)
  (GL11/glDepthFunc GL11/GL_LEQUAL)
  (GL11/glHint GL11/GL_PERSPECTIVE_CORRECTION_HINT GL11/GL_NICEST)
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  ;; Lighting
  ;(GL11/glEnable GL11/GL_LIGHTING)
  ;(GL11/glEnable GL11/GL_COLOR_MATERIAL)
  ;(GL11/glColorMaterial GL11/GL_FRONT_AND_BACK GL11/GL_AMBIENT_AND_DIFFUSE)
  ;(GL11/glLight GL11/GL_LIGHT0 GL11/GL_POSITION (float-buffer 5 5 0 1))
  ;(GL11/glEnable GL11/GL_LIGHT0)
  (load-textures)
  )

(defn- init-window [w h]
  (Display/setDisplayMode (DisplayMode. w h))
  (Display/setTitle "FPS")
  (Display/setVSyncEnabled true)
  (Display/create)
  (Mouse/setGrabbed true))

(defn run []
  (init-window 800 600)
  (init-gl)
  (loop [last-time (get-time)
         game (->Game
                (->Camera 5 2 10 0 0)
                [(->Box 5 5 -5 10 10 10 @crate-texture)])]
    (if (Display/isCloseRequested)
      (System/exit 0)
      ;(Display/destroy)
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
