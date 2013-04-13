(ns fps.component-entity)

(defmacro defcomponent [component-name & default-params]
  `(defn ~component-name [& params#]
     (merge ~(apply hash-map default-params) (apply hash-map params#))))

(defmacro entity [entity-name & components]
  (apply merge {:id entity-name}
         (map (fn [[component-name# & params#]]
                {(keyword component-name#) (apply hash-map params#)})
              components)))

(defcomponent position
  :x 0
  :y 0
  :z 0)

(defcomponent orient
  :pitch 0
  :yaw 0
  :roll 0)

(defcomponent velocity
  :vx 0
  :vy 0
  :vz 0)

(defcomponent volume
  :width 0
  :height 0
  :depth 0)

(defcomponent texture
  :texture nil)

(defcomponent render
  :fn #())

(defcomponent flight
  :airborn false)
