(ns fps.utils
  (:import [org.lwjgl BufferUtils]))

(defn float-buffer [& args]
  (-> (BufferUtils/createFloatBuffer (count args))
      (.put (float-array args))
      (.flip)))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn indexed [coll]
  (keep-indexed (fn [i e] [i e]) coll))

(defn select-indices [coll indices]
  (let [selected-map (select-keys coll indices)]
    (into [] (map #(get selected-map %) indices))))

(defn vassoc-in [m [k & ks] v]
  (if ks
    (assoc m k (vassoc-in (get m k []) ks v))
    (assoc m k v)))

(defn safe-subvec [v start end]
  (let [len (count v)]
    (cond
      (< end 0) []
      (> start len) []
      :else (subvec v (max start 0) (min end len)))))
