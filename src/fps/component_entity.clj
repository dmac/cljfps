(ns fps.component-entity)

(defmacro entity [entity-name & components]
  `(reduce add-component {:id ~entity-name} ~(vec components)))

(defmacro defcomponent [component-name & default-params]
  `(defn ~component-name [& params#]
     (let [spec# (merge ~(apply hash-map default-params) (apply hash-map params#))
           init-fn# (get spec# :init-fn)
           attributes# (dissoc spec# :init-fn)]
       (with-meta {:id ~(keyword component-name) :attributes attributes#}
                  {:init-fn init-fn#}))))

(defn add-component [entity component]
  (let [metadata (meta component)]
    (-> entity
        (assoc (:id component) (with-meta (:attributes component) metadata))
        (#(if (fn? (:init-fn metadata))
            ((:init-fn metadata) %)
            %)))))

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

(defcomponent material
  :type nil)

(defcomponent render
  :fn #())

(defcomponent flight
  :airborn false)
