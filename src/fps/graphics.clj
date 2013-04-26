(ns fps.graphics
  (:require [fps.textures :as textures]
            [fps.systems :as systems])
  (:use [fps.utils :only [select-indices float-buffer]])
  (:import [org.lwjgl.opengl Display DisplayMode GL11 GL15]
           [org.lwjgl BufferUtils]
           [org.lwjgl.util.glu GLU]
           [org.lwjgl.input Mouse]))

(def ^:private window-width 1024)
(def ^:private window-height 768)

(defn init-vertex-buffer []
  (let [int-buffer (BufferUtils/createIntBuffer 1)]
    (GL15/glGenBuffers int-buffer)
    (.get int-buffer 0)))

(defn clear-screen []
  (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
  (GL11/glLoadIdentity))

; TODO: To make this a little more sane, maybe use two VBOs instead of one interleaved VBO.
(defn regenerate-box-vertex-data! [box]
  (let [points (systems/bounding-points box)
        face-indices [[0 1 2 3] [4 5 1 0] [7 6 5 4] [3 2 6 7] [3 7 4 0] [1 5 6 2]]
        texture-points [[0 0] [0 1] [1 1] [1 0]]
        vertex-data (->> face-indices
                         (map (partial select-indices points))
                         (map #(interleave % texture-points))
                         (apply concat)
                         (apply concat)
                         (apply float-buffer))]
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER (get-in box [:render :vertex-buffer-id]))
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER vertex-data GL15/GL_STATIC_DRAW)
    box))

; TODO: glDrawArrays is a bottleneck. Rendering in chunks should improve performance. Maybe one big chunk?
(defn draw-box [box]
  ; [x y z tx ty]
  (let [sizeof-float 4
        vertex-size 3
        texture-size 2
        stride (* sizeof-float (+ vertex-size texture-size))
        vertex-offset 0
        texture-offset (* vertex-size sizeof-float)
        num-vertices 24]
    ; Consider having each face be its own VBO and only draw visible ones
    (GL11/glPushMatrix)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D (textures/get-texture-id (get-in box [:material :material])))
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER (get-in box [:render :vertex-buffer-id]))
    (GL11/glVertexPointer vertex-size GL11/GL_FLOAT stride vertex-offset)
    (GL11/glTexCoordPointer texture-size GL11/GL_FLOAT stride texture-offset)
    (GL11/glDrawArrays GL11/GL_QUADS 0 num-vertices)
    (GL11/glPopMatrix)))

(defn look-through [{{:keys [x y z]} :position {:keys [pitch yaw]} :orient :as entity}]
  {:pre [(and (:position entity) (:orient entity))]}
  (GL11/glRotatef pitch 1 0 0)
  (GL11/glRotatef yaw 0 1 0)
  ; The 0.8 adjustment increases the height of the camera "eyes" to near the top of the player entity.
  (GL11/glTranslatef (- x) (- (+ y 0.8)) (- z)))

(defn init-window []
  (Display/setDisplayMode (DisplayMode. window-width window-height))
  (Display/setTitle "FPS")
  (Display/setVSyncEnabled true)
  (Display/create)
  (Mouse/setGrabbed true))

(defn init-gl []
  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glLoadIdentity)
  (GLU/gluPerspective 70.0 (/ window-width window-height) 0.1 1000)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClearDepth 1)
  (GL11/glEnable GL11/GL_DEPTH_TEST)
  (GL11/glDepthFunc GL11/GL_LEQUAL)
  (GL11/glHint GL11/GL_PERSPECTIVE_CORRECTION_HINT GL11/GL_NICEST)
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (GL11/glEnableClientState GL11/GL_TEXTURE_COORD_ARRAY)
  ;; Lighting
  ;(GL11/glEnable GL11/GL_LIGHTING)
  ;(GL11/glEnable GL11/GL_COLOR_MATERIAL)
  ;(GL11/glColorMaterial GL11/GL_FRONT_AND_BACK GL11/GL_AMBIENT_AND_DIFFUSE)
  ;(GL11/glLight GL11/GL_LIGHT0 GL11/GL_POSITION (float-buffer 5 5 0 1))
  ;(GL11/glEnable GL11/GL_LIGHT0)
  (textures/load-textures))
