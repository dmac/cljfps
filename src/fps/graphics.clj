(ns fps.graphics
  (:require [fps.textures :as textures]
            [fps.systems :as systems])
  (:use [fps.textures :only [get-texture]]
        [fps.utils :only [select-indices]])
  (:import [org.lwjgl.opengl Display DisplayMode GL11]
           [org.lwjgl.util.glu GLU]
           [org.lwjgl.input Mouse]))

(def ^:private window-width 1024)
(def ^:private window-height 768)

(defn clear-screen []
  (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
  (GL11/glLoadIdentity))

(defn- set-color [[r g b]]
  (GL11/glColor3f r g b))

(defn draw-box [{{:keys [texture]} :texture :as box}]
  (set-color [1.0 1.0 1.0])
  (when texture
    (GL11/glBindTexture GL11/GL_TEXTURE_2D (.getTextureID (get-texture texture))))
  (GL11/glPushMatrix)
  (GL11/glBegin GL11/GL_QUADS)
  (let [points (systems/bounding-points box)
        texture-points [[0 0] [0 1] [1 1] [1 0]]]
    (doseq [point-indices [[0 1 2 3] [4 5 1 0] [7 6 5 4] [3 2 6 7] [3 7 4 0] [1 5 6 2]]]
      (doseq [[[tx ty] [x y z]] (partition 2 (interleave texture-points
                                                         (select-indices points point-indices)))]
        (GL11/glTexCoord2f tx ty)
        (GL11/glVertex3f x y z))))
  (GL11/glEnd)
  (GL11/glPopMatrix)
  (GL11/glBindTexture GL11/GL_TEXTURE_2D 0))

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
  ;; Lighting
  ;(GL11/glEnable GL11/GL_LIGHTING)
  ;(GL11/glEnable GL11/GL_COLOR_MATERIAL)
  ;(GL11/glColorMaterial GL11/GL_FRONT_AND_BACK GL11/GL_AMBIENT_AND_DIFFUSE)
  ;(GL11/glLight GL11/GL_LIGHT0 GL11/GL_POSITION (float-buffer 5 5 0 1))
  ;(GL11/glEnable GL11/GL_LIGHT0)
  (textures/load-textures))
