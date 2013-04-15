(ns fps.util
  (:import [java.nio ByteBuffer ByteOrder]))

(defn float-buffer [& args]
  (-> (doto (ByteBuffer/allocateDirect 16) (.order (ByteOrder/nativeOrder)))
      .clear .asFloatBuffer
      (.put (float-array args))
      .flip))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn indexed [coll]
  (keep-indexed (fn [i e] [i e]) coll))

(defn select-indices [coll indices]
  (let [selected-map (select-keys coll indices)]
    (into [] (map #(get selected-map %) indices))))
