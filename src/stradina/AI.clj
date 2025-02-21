(ns stradina.AI
  (:gen-class)
  (:use [clojure.repl]
        [clojure.java.shell :only [sh]]
        [stradina.util])
  (:require [clojure.string :as str]))

;; model constants
;; https://ollama.com/download
(defonce +ollama+ "ollama")
(def +ollama-model+ "deepseek-r1")

(defn ask-AI-model-question-fg [question program model fixup-fn]
  (let [result
        (condp = program
          +ollama+ (sh "/bin/bash" "-c" (format "echo \"%s\" | %s run %s" question program model))
          (error "Invalid program '%s'" program))]
    (if (= (:exit result) 0)
      (do
        (println)
        (println (fixup-fn (wrap-text (:out result) 80))))
      (errorln "ask-AI-model-question ('%s' '%s' '%s') failed with exit code %s"
               question program model (:exit result)))))

(defn ask-AI-model-question [question program model fixup-fn]
  (println "(Processing in a background thread.)")
  (future (ask-AI-model-question-fg question program model fixup-fn)))

;;
;; ------ OLLAMA ------
;;

(defn ask-ollama [question]
  (ask-AI-model-question question +ollama+ +ollama-model+
                         (fn [string]
                           (str/replace string "</think>" "</think>\n\n"))))

(defn ollama-simply-interpret [object object-desc]
  (ask-ollama (str "Please simply interpret the data contained within the following "
                   (format "%s: %s" object-desc (pr-str object)))))

(defn ollama-interpret-map [map]
  (assert (map? map))
  (ollama-simply-interpret map "stringified Clojure map/record"))

(defn ollama-interpret-list-of-maps [li]
  (assert (and (list? li) (every? map? li)))
  (ollama-simply-interpret li "stringified Clojure list of maps/records"))

(defn ollama-interpret-movement-data [li]
  (assert (and (list? li) (every? map? li)))
  (ollama-simply-interpret li "stringified Clojure list of maps/records representing movement records"))