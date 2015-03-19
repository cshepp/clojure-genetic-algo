(ns evo.genetics)

(def mutation-chance 15)
(def num-participants 4)
(def mixing-ratio (/ 4 5))
(def distances
  "{[start_city ->  end_city]  distance}"
  {[0 1] 25 ;; 20
   [0 2] 42
   [0 3] 35
   [1 0] 25 ;; 20
   [1 2] 30
   [1 3] 34
   [2 0] 42
   [2 1] 30
   [2 3] 12
   [3 0] 35
   [3 1] 34
   [3 2] 12})

(defn unique-random-numbers [n]
  "Generates a list of unique random ints between 0 and n"
  (let [a-set (set (take n (repeatedly #(rand-int n))))]
    (concat a-set (clojure.set/difference (set (take n (range)))
                                  a-set))))

(defn pathify [vec]
  "turns a vector into a vector of n-grams,
   including last element back to the first

   ex: [0 1 2] -> [[0 1] [1 2] [2 0]]"
  (map (fn [i] (subvec (into vec vec) i (+ 2 i))) (range (count vec))))

(defn swap [v i1 i2]
  "swaps two elements in a vector"
  (assoc v i2 (v i1) i1 (v i2)))

(defn pair [vec]
  "splits a 1d array into a 2d array of pairs"
  (mapv (fn [i] (subvec vec i (+ 2 i)))
        (map #(* 2 %)
             (range (/ (count vec) 2)))))

(defn calculate-fitness [genome]
  (let [path (pathify genome)
        distances (map #(get distances %) path)]
    (reduce + distances)))

;; doesn't guarantee that the offspring are valid genomes
;; (they can have duplicate cities)
(defn single-point-crossover [mates]
  ""
  (let [i 1
        mate1 (mates 0)
        mate2 (mates 1)
        a (subvec mate1 0 (inc i))
        b (subvec mate1 (inc i) (count mate1))
        c (subvec mate2 0 (inc i))
        d (subvec mate2 (inc i) (count mate2))
        child1 (into a c)
        child2 (into b d)]
    [child1 child2]))

(defn uniform-crossover [mates]
  (let [num-swap (int (* mixing-ratio (count (first mates))))
        swap-idxs (take num-swap (unique-random-numbers (count (first mates))))
        ]
    swap-idxs))

(uniform-crossover [[0 1 2 3] [2 3 1 0]])

(defn should-mutate? []
  (< (rand-int 100) mutation-chance))

(defn mutate [genome]
  (let [a (rand-int (count genome))
        b (rand-int (count genome))]
    (swap genome a b)))

(defn create-population [gen-size genome-size]
  (map (fn [x] (vec (unique-random-numbers genome-size)))
       (range gen-size)))

(defn tournament-select [population]
  ""
  (mapv (fn [x] (let [idxs (repeatedly num-participants #(rand-int (count pop)))
                     participants (map #(population %) idxs)]
                 (first (sort-by calculate-fitness participants))))
       (range (count population))))

(defn next-generation [population]
  (->> (sort-by calculate-fitness population)
        vec
        tournament-select
        pair
       (map uniform-crossover)
       (reduce into)
       (map (fn [x] (if (should-mutate?)
                       (mutate x)
                       x)))))

(defn evolve [max-gen gen-size]
  (loop [population (create-population gen-size 4)
         cur-gen    0]
    (if (< cur-gen max-gen)
      (let [ngen (next-generation population)
            gen-num (inc cur-gen)]
        (do (println (str "Gen: " gen-num " -> " (prn-str ngen)))
            (recur ngen gen-num)))
      population)))

(defn solve [max-gen gen-size]
  (let [final-population (evolve max-gen gen-size)]
    (first (sort-by calculate-fitness final-population))))

#_(solve 1000 100)


(uniform-crossover [[0 2 1 3] [2 3 1 0]])

 (subvec [0 1 2 3] 0 (inc 1))
 (subvec [0 1 2 3] (inc 1) (count [0 1 2 3]))

