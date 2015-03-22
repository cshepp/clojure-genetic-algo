(ns evo.core
  (:require [clojure.core.async :refer [chan]]
            [evo.genetics :as g]
            [evo.draw :as d]))

(defn main []
  (let [ch (chan)
        max-gen  1
        gen-size 100]
    (d/init ch)
    (g/solve max-gen gen-size ch)))

#_(main)
