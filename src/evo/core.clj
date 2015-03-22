(ns evo.core
  (:require [clojure.core.async :refer [chan]]
            [evo.genetics :as g]
            [evo.draw :as d]))

(defn main []
  (let [ch (chan)
        max-gen  150
        gen-size 400]
    (d/init max-gen gen-size ch)
    (g/solve max-gen gen-size ch)))

#_(main)
