;; cache.clj is responsible for local data caching, primarily:
;;   - Google Directions API directions data
;;   - Google Places API autocompletion data
;;
;; The caching system works efficiently by writing session data to a file called ./cache.next,
;; which is merged into the main ./cache file on the next startup. Writes will be highly efficient
;; at first and become slightly less efficient as more data is written, until the REPL is restarted.

(ns stradina.cache
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:use [stradina.util]))

(defn get-cache []
  (if (or (file-exists? "./cache") (file-exists? "./cache.next"))
    (do
      (let [new-cache (merge (read-map-from-file? "./cache") (read-map-from-file? "./cache.next"))]
        (write-map-to-file new-cache "./cache")
        (delete-file? "./cache.next")
        (println "Loaded the cache.")
        new-cache))
    {}))

(defonce cache (atom (get-cache)))
(defonce cache-next (atom {}))

(defn cache! []
  (merge @cache @cache-next))

(defn reload-cache []
  (swap! cache (fn [cache] (get-cache))))

(defn write-cache-to-file []
  (write-map-to-file @cache-next "./cache.next")
  (write-map-to-file @cache "./cache.back.auto"))

(defn backup-cache []
  (write-map-to-file @cache "./cache.back.manual"))

(defn cache-get [k]
  (get (cache!) k))

(defn cache-assoc [k v]
  (swap! cache-next (fn [cache] (assoc cache k v)))
  (write-cache-to-file)
  (cache!))

(defn cache-assoc? [k v]
  (if-not (cache-get k)
    (cache-assoc k v)))

(defn cache-dissoc [k]
  (swap! cache-next (fn [cache] (dissoc cache k)))
  (write-cache-to-file))