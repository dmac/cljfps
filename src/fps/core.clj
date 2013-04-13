(ns fps.core
  (:use [fps.component-entity :only [entity]])
  (:import [java.nio ByteBuffer ByteOrder]
           [org.lwjgl Sys]
           [org.lwjgl.opengl Display DisplayMode GL11]
           [org.lwjgl.util.glu GLU]
           [org.lwjgl.input Keyboard Mouse]
           [org.newdawn.slick.opengl TextureLoader]
           [org.newdawn.slick.util ResourceLoader])
  (:gen-class))

; TODO
; - collision detection with AABBs

(def movement-speed 10) ; meters per second
(def mouse-sensitivity 0.1)

(def crate-texture (atom nil))

(defn- find-first [pred coll]
  (first (filter pred coll)))

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

(defn- bounding-points [{{:keys [x y z]} :position
                         {:keys [width height depth]} :volume :as entity}]
  {:pre [(and (:position entity) (:volume entity))]}
  (let [hw (/ width 2) hh (/ height 2) hd (/ depth 2)]
    [[(- x hw) (- y hh) (+ z hd)] [(- x hw) (+ y hh) (+ z hd)]
     [(+ x hw) (+ y hh) (+ z hd)] [(+ x hw) (- y hh) (+ z hd)]
     [(- x hw) (- y hh) (- z hd)] [(- x hw) (+ y hh) (- z hd)]
     [(+ x hw) (+ y hh) (- z hd)] [(+ x hw) (- y hh) (- z hd)]]))

(defn- collides? [{{x1 :x y1 :y z1 :z} :position {w1 :width h1 :height d1 :depth} :volume :as entity1}
                  {{x2 :x y2 :y z2 :z} :position {w2 :width h2 :height d2 :depth} :volume :as entity2}]
  {:pre [(and (:position entity1) (:volume entity1))
         (and (:position entity2) (:volume entity2))]}
  (and (< (Math/abs (- x1 x2)) (+ (/ w1 2) (/ w2 2)))
       (< (Math/abs (- y1 y2)) (+ (/ h1 2) (/ h2 2)))
       (< (Math/abs (- z1 z2)) (+ (/ d1 2) (/ d2 2)))))

(defn- draw-box [{{:keys [texture]} :texture :as box}]
  (set-color [1.0 1.0 1.0])
  (when texture
    (GL11/glBindTexture GL11/GL_TEXTURE_2D (.getTextureID texture)))
  (GL11/glPushMatrix)
  (GL11/glBegin GL11/GL_QUADS)
  (let [points (bounding-points box)
        texture-points [[0 0] [0 1] [1 1] [1 0]]]
    (doseq [point-indices [[0 1 2 3] [4 5 1 0] [7 6 5 4] [3 2 6 7] [3 7 4 0] [1 5 6 2]]]
      (doseq [[[tx ty] [x y z]] (partition 2 (interleave texture-points
                                                         (select-indices points point-indices)))]
        (GL11/glTexCoord2f tx ty)
        (GL11/glVertex3f x y z))))
  (GL11/glEnd)
  (GL11/glPopMatrix)
  (GL11/glBindTexture GL11/GL_TEXTURE_2D 0))

(defn- draw [{:keys [entities]}]
  (doseq [entity (filter :render entities)]
    ((get-in entity [:render :fn]) entity)))

(defn- look-through [{{:keys [x y z]} :position {:keys [pitch yaw]} :orient}]
  (GL11/glRotatef pitch 1 0 0)
  (GL11/glRotatef yaw 0 1 0)
  (GL11/glTranslatef (- x) (- y) (- z)))

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
  {:pre [(and (:position entity))]}
  (let [new-entity (update-in entity [:position :y] + distance)]
    (if (some (partial collides? new-entity) collidables)
      (-> entity
          (assoc-in [:velocity :vy] 0)
          (assoc-in [:flight :airborn] false))
      new-entity)))

(defn- handle-mouse-input [game]
  (let [dx (Mouse/getDX)
        dy (- (Mouse/getDY))]
    (-> game
        (update-in [:player :orient :pitch] #(mod (+ % (* dy mouse-sensitivity)) 360))
        (update-in [:player :orient :yaw] #(mod (+ % (* dx mouse-sensitivity)) 360)))))

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
             (not (get-in game [:player :flight :airborn]))) (-> game
                                                                 (assoc-in [:player :flight :airborn] true)
                                                                 (assoc-in [:player :velocity :vy] 5))
        :else game))
    game
    key-buffer))

(defn- handle-constant-keyboard-input [game dt]
  (reduce
    (fn [game [k f]] (if (Keyboard/isKeyDown k) (f game) game))
    game
    [[Keyboard/KEY_A #(update-in % [:player] move :left (distance-traveled dt) (:entities game))]
     [Keyboard/KEY_D #(update-in % [:player] move :right (distance-traveled dt) (:entities game))]
     [Keyboard/KEY_S #(update-in % [:player] move :backward (distance-traveled dt) (:entities game))]
     [Keyboard/KEY_W #(update-in % [:player] move :forward (distance-traveled dt) (:entities game))]]))

(defn- tick [game dt]
  (let [y-distance (-> dt (/ 1000) (* (get-in game [:player :velocity :vy])))]
    (-> game
        (update-in [:player] move-vertical y-distance (:entities game))
        (#(if (> (get-in % [:player :position :y]) 2)
            (update-in % [:player :velocity :vy] + (* -10 (/ dt 1000)))
            (-> %
                (assoc-in [:player :position :y] 2)
                (assoc-in [:player :velocity :vy] 0)
                (assoc-in [:player :flight :airborn] false)))))))

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
  (init-window 1024 768)
  (init-gl)
  (loop [last-time (get-time)
         game {:player (entity :player
                         (position :x 5 :y 2 :z 10)
                         (volume :width 1 :height 2 :depth 1)
                         (orient :pitch 0 :yaw 0)
                         (velocity :vy 0)
                         (flight :airborn false))
               :entities [(entity :box
                            (position :x 5 :y 5 :z -5)
                            (volume :width 10 :height 10 :depth 10)
                            (texture :texture @crate-texture)
                            (render :fn draw-box))
                          (entity :box2
                            (position :x 0 :y 1 :z 2)
                            (volume :width 2 :height 2 :depth 2)
                            (texture :texture @crate-texture)
                            (render :fn draw-box))]}]
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
                           (tick dt))]
          (clear-screen)
          (look-through (:player game))
          (draw new-game)
          (Display/update)
          (Display/sync 60)
          (recur new-time new-game))))))

(defn -main []
  (run))
