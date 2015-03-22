(ns evo.genetics
  (:require [evo.util :refer :all]
            [evo.distances :refer [get-distance]]
            [clojure.core.async :refer [>! go chan]]))

(def num-cities 10)      ;; how many cities we have in our problem
(def mutation-chance 5) ;; how likely it is for a mutation to occur
(def fitness-multiplier 100)

;; initialize population
(defn create-population [gen-size genome-size]
  "creates a random population of size gensize,
   with each genome of size genome-size"
  (map (fn [x] (vec (unique-random-numbers genome-size)))
       (range gen-size)))

;; fitness
(defn calculate-fitness [genome]
  "Returns the total distance travelled for a given genome"
  (let [path (pathify genome)
        distances (map get-distance path)]
    (reduce + distances)))

;; crossover
(defn find-cycle [a b seed]
  "finds a single cycle in the parents
   starting at the seed index"
  (loop [coll (mapv vector a b)
         cyl  #{}
         cur  (get-in coll [seed 0])]
    (let [idx (first (keep-indexed #(if (= (first %2) cur) %1) coll))]
      (if (nil? idx)
        (vec cyl)
        (recur (vec-remove coll idx)
               (into cyl (nth coll idx))
               (last (nth coll idx)))))))

(defn find-all-cycles [a b]
  "finds all possible cycles in the parents"
  (loop [idx 0
         cyl (order-preserving-set)]
    (let [c (find-cycle a b idx)]
      (if (= idx (count a))
        (vec (seq cyl))
        (recur (inc idx) (into cyl [c]))))))


(defn make-child [parents cycles]
  "combines parents using cycle crossover"
  (loop [res (vec (repeat 10 nil))
         idx 0]
    (if (= idx (count cycles))
        res
        (recur (reduce (fn [v e] (assoc v (first e) (last e)))
                       res
                       (map (fn [i] [(.indexOf (nth parents (mod idx 2)) i) i])
                            (nth cycles idx)))
               (inc idx)))))

(defn cycle-crossover [[a b]]
  "returns both permuations of the parents'
   cycle crossover (changed ordering)"
  (let [cycles (find-all-cycles a b)
        c1     (make-child [a b] cycles)
        c2     (make-child [b a] cycles)]
    [c1 c2]))

;; mutation
(defn should-mutate? []
  "determines whether or not a given genome
   should be mutated"
  (< (rand-int 100) mutation-chance))

(defn mutate [genome]
  "swaps two sets of random elements in the given genome"
  (let [a (rand-int (count genome))
        b (rand-int (count genome))
        c (rand-int (count genome))
        d (rand-int (count genome))]
    (swap (swap genome c d) a b)))

;; selection
(defn roulette-select [population]
  "selects genomes based on fitness"
  (let [roulette-wheel  (reduce #(conj %1 (+ (* (- 700 %2) fitness-multiplier) (last %1))) [0] (map calculate-fitness population))
        total           (last roulette-wheel)]
    (mapv (fn [x] (nth population (.indexOf roulette-wheel (first (filter #(< (rand-int total) %1) roulette-wheel)))))
          (range (count population)))))

(defn select-elites [old new]
  "replaces the worst 10 of the new generation
  with the best 10 of the old generation"
  (let [best  (take 10 (sort-by calculate-fitness old))]
    (into (drop-last 10 new) best)))

;; single-generation cycle: select -> crossover -> mutate
(defn next-generation [population]
  ""
  (->> (sort-by calculate-fitness population)
        vec
        ;;roulette-select
        pair
       (map cycle-crossover)
       (reduce into)
       (map (fn [x] (if (should-mutate?)
                       (mutate x)
                       x)))
       (select-elites population)
       ))

;; full evolution cycle (many generations)
(defn evolve [max-gen gen-size draw-chan]
  "performs the generation cycle for
   [max-gen] number of generations"
  (loop [population (create-population gen-size num-cities)
         cur-gen    0]
    (if (< cur-gen max-gen)
      (let [ngen     (next-generation population)
            gen-num  (inc cur-gen)
            best-fit (calculate-fitness (first (sort-by calculate-fitness ngen)))
            avg-fit  (float (/ (reduce + (map calculate-fitness ngen)) (count ngen)))]
        (do
          (go
           ;;(>! draw-chan (mapv calculate-fitness ngen)))
           (>! draw-chan (mapv calculate-fitness (sort-by calculate-fitness ngen))))
          (recur ngen gen-num)))
      population)))

(defn solve [max-gen gen-size draw-chan]
  "evolves and then extracts the fittest
   genome from the final generation"
  (let [final-population (evolve max-gen gen-size draw-chan)]
    (calculate-fitness (first (sort-by calculate-fitness final-population)))))

#_(solve 1 1 (chan))
#_(>! draw-chan (mapv (fn [x] (Long/parseLong (reduce #(str %1 %2) "" x))) ngen))
#_(>! draw-chan (mapv calculate-fitness (sort-by calculate-fitness ngen)))
