(ns evo.core
  (:require [evo.genetics :as g]
            [evo.draw :as d]))

(defn main []
  (let [ch (chan)
        sc (d/start-screen)
        max-gen  100
        gen-size 100]
    (d/draw-loop ch sc)
    (g/solve max-gen gen-size ch)))

#_(main)


