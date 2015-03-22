(ns evo.draw
  (:require [clojure.core.async :refer [go <!]]
            [quil.core :as q]))

(defn setup []
  (q/smooth)
  (q/frame-rate 1000))

(defn rgb->hex [[r g b]]
  (Long/toHexString (+ b (* g 256) (* r 65536))))

(defn rgb [mn mx val]
  (let [r (/ (* 2 (- val mn)) (- mx mn))
        b (int (max 0 (* 255 (- 1 r))))
        r (int (max 0 (* 255 (- r 1))))
        g (- (- 255 b) r)]
    [r g b]))

(defn get-color [value max]
  (let [c (rgb 0 max value)]
    (str (rgb->hex c))))

(defn draw-square [x y v]
  (q/fill (q/random 255))
  (q/rect x y 10 10))

(defn draw-row [x row]
  (doall (map-indexed #(draw-square x %1 %2) row)))

(defn draw-screen [rows]
  (doall (map-indexed #(draw-row %1 %2) rows)))

(defn init [ch]
  (defn draw []
    (go
      (let [row (<! ch)]
        (do
          (println (str "draw loop -> " (count row)))
          (draw-row 0 row)))))
  (q/defsketch evolution
    :title "test"
    :setup setup
    :draw  draw
    :size  [600 600]))
