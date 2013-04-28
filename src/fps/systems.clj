(ns fps.systems
  (:use [fps.utils :only [find-first safe-subvec]]))

(defn world-blocks [world]
  (->> world :grid (apply concat) (apply concat) (remove nil?)))

(defn nearby-blocks [{{:keys [x y z]} :position :as entity} world radius]
  {:pre [(:position entity)]}
  (let [get-min-max (juxt #(- % radius) #(+ % radius 1))
        [min-x max-x] (get-min-max x)
        [min-y max-y] (get-min-max y)
        [min-z max-z] (get-min-max z)
        xs (safe-subvec (:grid world) min-x max-x)
        xs-ys (map #(safe-subvec % min-y max-y) xs)
        xs-ys-zs (map (fn [ys-zs] (map #(safe-subvec % min-z max-z) ys-zs)) xs-ys)]
    (->> xs-ys-zs (apply concat) (apply concat) (remove nil?))))

(defn- collides? [{{x1 :x y1 :y z1 :z} :position {w1 :width h1 :height d1 :depth} :volume :as entity1}
                  {{x2 :x y2 :y z2 :z} :position {w2 :width h2 :height d2 :depth} :volume :as entity2}]
  {:pre [(and (:position entity1) (:volume entity1))
         (and (:position entity2) (:volume entity2))]}
  (and (<= (Math/abs (- x1 x2)) (+ (/ w1 2) (/ w2 2)))
       (<= (Math/abs (- y1 y2)) (+ (/ h1 2) (/ h2 2)))
       (<= (Math/abs (- z1 z2)) (+ (/ d1 2) (/ d2 2)))))

(defn bounding-points [{{:keys [x y z]} :position
                        {:keys [width height depth]} :volume :as entity}]
  {:pre [(and (:position entity) (:volume entity))]}
  (let [hw (/ width 2) hh (/ height 2) hd (/ depth 2)]
    [[(- x hw) (- y hh) (+ z hd)] [(- x hw) (+ y hh) (+ z hd)]
     [(+ x hw) (+ y hh) (+ z hd)] [(+ x hw) (- y hh) (+ z hd)]
     [(- x hw) (- y hh) (- z hd)] [(- x hw) (+ y hh) (- z hd)]
     [(+ x hw) (+ y hh) (- z hd)] [(+ x hw) (- y hh) (- z hd)]]))

(defn move [entity direction distance collidables]
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

(defn update-flight [game dt]
  (reduce
    (fn [game entity-index]
      (let [acceleration -20
            dt-seconds (/ dt 1000)
            y-delta (+ (* dt-seconds (get-in game (concat entity-index [:velocity :vy])))
                       (* acceleration 0.5 dt-seconds dt-seconds))
            vy-delta (* acceleration dt-seconds)
            collidables (concat (vals (dissoc (:entities game) (last entity-index)))
                                (nearby-blocks (get-in game entity-index) (:world game) 3))]
        (-> game
            (update-in (concat entity-index [:velocity :vy]) + vy-delta)
            (update-in entity-index move-vertical y-delta collidables))))
    game
    (for [entity-id (keys (:entities game))
          :let [entity (get-in game [:entities entity-id])]
          :when (and (:flight entity) (:velocity entity))]
      [:entities entity-id])))
