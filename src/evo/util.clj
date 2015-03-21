(ns evo.util)

(defn unique-random-numbers [n]
  "Generates a list of unique random ints between 0 and n"
  (let [a-set (set (take n (repeatedly #(rand-int n))))]
    (concat a-set (clojure.set/difference (set (take n (range)))
                                  a-set))))

(defn pathify [vec]
  "turns a vector into a vector of n-grams,
   including last element back to the first

   ex: [0 1 2] -> [[0 1] [1 2] [2 0]]"
  (map (fn [i]
           (subvec (into vec vec) i (+ 2 i)))
       (range (count vec))))

(defn swap [v i1 i2]
  "swaps two elements in a vector"
  (assoc v i2 (v i1) i1 (v i2)))

(defn pair [vec]
  "splits a 1d array into a 2d array of pairs"
  (mapv (fn [i] (subvec vec i (+ 2 i)))
        (map #(* 2 %)
             (range (/ (count vec) 2)))))

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

;; from http://stackoverflow.com/questions/1394991/clojure-remove-item-from-vector-at-a-specified-location
(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))
