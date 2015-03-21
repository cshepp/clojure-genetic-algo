(ns evo.orderedset)


;; from https://groups.google.com/forum/#!topic/clojure/vYND1CtKf2M
(deftype OrderPreservingSet [v m s]
  clojure.lang.IObj
    (withMeta [this md] (OrderPreservingSet. v (with-meta m md) s))
    (meta [this] (meta m))
  clojure.lang.IPersistentSet
    (cons [this object]
      (if (contains? m object)
        this
        (OrderPreservingSet.
          (conj v object)
          (assoc m object (count v))
          s)))
    (count [this] (count m))
    (empty [this] (OrderPreservingSet. [] {} (Object.)))
    (equiv [this other] (= (seq this) other))
    (seq [this] (seq (remove #{s} v)))
    (get [this object]
      (v (m object)))
    (contains [this object]
      (contains? m object))
    (disjoin [this object]
      (if-let [idx (m object)]
        (OrderPreservingSet.
          (assoc v idx s)
          (dissoc m object)
          s)
       this))
  clojure.lang.IFn
    (invoke [this object]
      (get this object))
    (invoke [this object not-found]
      (get this object not-found)))

(defn order-preserving-set [& things]
  (reduce conj (OrderPreservingSet. [] {} (Object.)) things))
