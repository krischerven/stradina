;; store.clj is responsible for all local data handling:
;;   - API keys
;;   - Configuration settings
;;   - Walk data points

(ns stradina.store
  (:gen-class)
  (:require [clojure.string :as str])
  (:use [stradina.util]))

(defn get-store []
  (if (file-exists? "./store")
    (do
      (let [new-store (read-map-from-file "./store")]
        (formatln "Loaded %d parameters from local storage." (count (keys new-store)))
        (formatln " - Loaded %d movement data points." (count (get new-store :movement-data)))
        new-store))
    {}))

(defonce store (atom (get-store)))

(defn reload-store []
  (swap! store (fn [store] (get-store))))

(defn write-store-to-file []
  (write-map-to-file @store "./store")
  (write-map-to-file @store "./store.back.auto"))

(defn backup-store []
  (write-map-to-file @store "./store.back.manual"))

(defn store-assoc [k v]
  (letfn [(body [] ((swap! store (fn [store] (assoc store k v)))
                    (write-store-to-file)
                    store))]
    (cond (and (= k :city) (< (count (str/split v #",")) 2))
          (when (yesno (str "Storing :city without a state/province/country name (eg: Detroit, MI) is not recommended."
                            "\nContinue anyway?"))
            (body))
          :else (body))))

(defn store-assoc? [k v]
  (if-not (get @store k)
    (store-assoc k v)))

(defn store-dissoc [k]
  (swap! store (fn [store] (dissoc store k)))
  (write-store-to-file)
  store)

(defn store-swap [k]
  (let [v (get @store k)]
    (if (boolean? v)
      (do
        (store-assoc k (not v))
        nil)
      (error "store-swap (%s): '%s' is not a boolean" k v))))

(defn store-rename-key [k1 k2]
  (if-let [v (get @store k1)]
    (do
      ;; Avoid superfluous I/O
      (swap! store (fn [store] (assoc store k2 v)))
      (store-dissoc k1))
    (errorln "store-rename-key: No such key '%s'" k1)))

(defn store-get [k]
  (get @store k))

(defn store-get! [k]
  (let [v (get @store k)]
    (if v
      v
      (error "store-get!: Missing key '%s'." k))))

;; Storing API keys
(defn add-API-key [name API-key]
  (store-assoc :API-keys (assoc (or (store-get :API-keys) {}) name API-key)))

(defn get-API-key? [key-name]
  (get (store-get :API-keys) key-name))

(defn get-API-key [key-name]
  (if-let [api-key (get-API-key? key-name)]
    api-key
    (error "Missing API key %s. Please add it via (add-API-key %s \"KEY-GOES-HERE\")"
           key-name key-name)))

(defn list-API-keys []
  (store-get :API-keys))

(def +cached-get-directions-data+ "cached get-directions data")
(def +cached-autocomplete-data+ "cached autocomplete data")

;; Storing directions
(defn store-invalidate-all-directions []
  (swap! store (fn [store] (into {} (filter #(or
                                          (not (map? (second %)))
                                          (not (get (second %) :type))
                                          (not= (get (second %) :type) +cached-get-directions-data+))
                                        (seq store)))))
  (write-store-to-file)
  store)

(defn store-invalidate-all-autocomplete-data []
  (swap! store (fn [store] (into {} (filter #(or
                                          (not (map? (second %)))
                                          (not (get (second %) :type))
                                          (not= (get (second %) :type) +cached-autocomplete-data+))
                                        (seq store)))))
  (write-store-to-file)
  store)

;; Storing movement data
(defn add-walk-data-point [tag meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                      {:tag tag
                                       :type :walk
                                       :time (current-timestamp)
                                       :meters meters
                                       :seconds seconds}))
  nil)

(defn add-jog-data-point [tag meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                      {:tag tag
                                       :type :jog
                                       :time (current-timestamp)
                                       :meters meters
                                       :seconds seconds}))
  nil)

(defn add-run-data-point [tag meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                      {:tag tag
                                       :type :run
                                       :time (current-timestamp)
                                       :meters meters
                                       :seconds seconds}))
  nil)

(defn add-walk-data-point-with-note [tag note meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                 {:tag tag
                                  :type :walk
                                  :note note
                                  :time (current-timestamp)
                                  :meters meters
                                  :seconds seconds})))

(defn add-jog-data-point-with-note [tag note meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                 {:tag tag
                                  :type :jog
                                  :note note
                                  :time (current-timestamp)
                                  :meters meters
                                  :seconds seconds})))

(defn add-run-data-point-with-note [tag note meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                 {:tag tag
                                  :type :run
                                  :note note
                                  :time (current-timestamp)
                                  :meters meters
                                  :seconds seconds})))

(defn movement-data []
  (map #(assoc % :meters-per-second (/ (:meters %) (:seconds %) 1.0)) (store-get :movement-data)))

(defn remove-movement-data-where-tag= [tag]
  (store-assoc :movement-data (filter #(not= (:tag %) tag) (movement-data))))

(defn update-movement-data-point [idx k v]
  (store-assoc :movement-data
               (seq (assoc (vec (movement-data)) idx (assoc (nth (movement-data) idx) k v)))))

(defn update-all-movement-data-points [k v]
  (store-assoc :movement-data
               (vec (map #(assoc % k v) (movement-data)))))

(defn walk-data []
  (filter #(= (:type %) :walk) (movement-data)))

(defn jog-data []
  (filter #(= (:type %) :jog) (movement-data)))

(defn run-data []
  (filter #(= (:type %) :run) (movement-data)))

(defn walk-data-idx []
  (map-indexed #(assoc %2 :idx %1) (walk-data)))

(defn jog-data-idx []
  (map-indexed #(assoc %2 :idx %1) (jog-data)))

(defn run-data-idx []
  (map-indexed #(assoc %2 :idx %1) (run-data)))