(ns evo.draw
  (:require [lanterna.screen :as s]))

(defn start-screen []
  (-> (s/get-screen)
      s/start))

(defn draw-loop [ch sc])
