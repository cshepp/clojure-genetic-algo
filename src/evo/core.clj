(ns evo.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async :refer [chan]]
            [clojure.string :as string]
            [evo.genetics :as g]
            [evo.draw :as d])
  (:gen-class))

(defn main [max-gen gen-size]
  (let [ch (chan)]
    (d/init max-gen gen-size ch)
    (g/solve max-gen gen-size ch)))

;; cli stuff below
(def cli-options
  [["-g" "--gensize SIZE" "Generation Size"
    :default 100
    :parse-fn #(Integer/parseInt %)]
   ["-m" "--maxgen MAX" "Maximum Generations"
    :default 100
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]])

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
       (:help options) (exit 0 summary)
       ;; todo - validate args
       errors (exit 1 (error-msg errors)))
    (main (:maxgen options) (:gensize options))))
