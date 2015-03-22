(ns evo.draw
  (:require [clojure.core.async :refer [<!!]]
            [quil.core :as q]))

(def size 2)

(defn setup []
  (q/smooth)
  (q/frame-rate 1000))

(defn rgb->hex [[r g b]]
  (Long/toHexString (+ b (* g 256) (* r 65536))))

(defn rgb [mn mx val]
  (let [ratio (/ (* 2 (- val mn)) (- mx mn))
        r     (int (max 0 (* 255 (- 1 ratio))))
        b     (int (max 0 (* 255 (- ratio 1))))
        g     (- (- 255 b) r)]
    [r g b]))

(defn get-color [value max]
  (let [c (rgb 0 max value)]
    (str (rgb->hex c))))

(defn draw-square [y x v]
  (q/fill (apply q/color (rgb 350 700 v)))
  (q/no-stroke)
  (q/rect (* size x) (* size y) size size))

(defn draw-row [y row]
  (doall (map-indexed #(draw-square y %1 %2) row)))

(defn draw-screen [rows]
  (doall (map-indexed #(draw-row %1 %2) rows)))

(def counter (atom 0))

(defn init [h w ch]
  (defn draw []
    (let [row (<!! ch)
          y   @counter]
      (do
        (draw-row y row)
        (reset! counter (inc y)))))
  (q/defsketch evolution
    :title ""
    :setup setup
    :draw  draw
    :size  [(* w size) (* h size)]))
