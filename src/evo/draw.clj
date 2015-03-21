(ns evo.draw
  (:require [lanterna.screen :as s]))

(defn start-screen []
  (let [sc (s/get-screen)]
      (s/start sc)
    sc))

(defn draw-loop [ch sc])
