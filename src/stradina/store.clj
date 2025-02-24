;; store.clj is responsible for all local data handling:
;;   - API keys
;;   - Configuration settings
;;   - Walk data points

(ns stradina.store
  (:gen-class)
  (:require [clojure.string :as str])
  (:use [stradina.util]))

(defn get-store
  "Loads ./store from memory and return it as a PersistentHashMap"
  []
  (if (file-exists? "./store")
    (do
      (let [new-store (read-map-from-file "./store")]
        (formatln "Loaded %d parameters from local storage." (count (keys new-store)))
        (formatln " - Loaded %d movement data points." (count (get new-store :movement-data)))
        new-store))
    {}))

(defonce store (atom (get-store)))

(defn reload-store
  "Swaps the value of the <store> atom to the result of (get-store)"
  []
  (swap! store (fn [store] (get-store))))

(def ^:dynamic ^:private *store-writes* true)
(defmacro without-store-writes
  "Disables all store writes-to-disk within a scope"
  [& body]
  `(binding [*store-writes* false]
     ~@body))

(defn write-store-to-file
  "Writes the value of the <store> atom to ./store (and ./store.back.auto)"
  []
  (when *store-writes*
    (write-map-to-file @store "./store")
    (write-map-to-file @store "./store.back.auto")))

(defn backup-store
  "Writes the value of the <store> atom to ./store.back.manual"
  []
  (write-map-to-file @store "./store.back.manual"))

(defn store-has
  "Returns true if store contains the key <k>, else returns false"
  [k]
  (contains? @store k))

(defn store-get
  "Gets the value of any key named <k> in the store"
  [k]
  (get @store k))

(defn store-get!
  "Gets the value of any key named <k> in the store. Errors if no such key exists"
  [k]
  (let [v (get @store k)]
    (if v
      v
      (error "store-get!: Missing key '%s'." k))))

(defn store-assoc
  "Associates <k> with <v> in the store, then writes the store to disk"
  [k v]
  (letfn [(body [] ((swap! store (fn [store] (assoc store k v)))
                    (write-store-to-file)
                    store))]
    (cond (and (= k :city) (< (count (str/split v #",")) 2))
          (when (yesno (str "Storing :city without a state/province/country name (eg: Detroit, MI) is not recommended."
                            "\nContinue anyway?"))
            (body))
          :else (body))
    nil))

(defmacro with-store-assoc
  "Associates <k> with <v> in the store, but only for the duration of the scope.
  This function is transactional and will never write anything to disk"
  [k v & body]
  "store-assocs <k> to <v> for the duration of <body>."
  (let [filledp (gensym)
        original (gensym)
        return (gensym)]
    `(let [~filledp (contains? @store ~k)
           ~original (store-get ~k)]
       (without-store-writes
        (store-assoc ~k ~v)
        (let [~return (do ~@body)]
          (if ~filledp
            (store-assoc ~k ~original)
            (store-dissoc ~k))
          ~return)))))

(defn store-assoc?
  "Runs (store-assoc <k> <v>) if <k> is not already in the store"
  [k v]
  (if-not (get @store k)
    (store-assoc k v)))

(defn store-dissoc
  "Removes any <k> that may or may not exist in the store"
  [k]
  (when (store-has k)
    (swap! store (fn [store] (dissoc store k)))
    (write-store-to-file))
  nil)

(defn store-swap
  "Swaps the value of (store-get <k>) with (not (store-get <k>)), then returns nil

  - If <k> is not in the store, this function shall error
  - If (store-get <k>) is not a boolean, this function shall error"
  [k]
  (if (not (store-has k))
    (error "store-swap (%s): No such key exists in the store." k)
    (let [v (get @store k)]
      (if (boolean? v)
        (do
          (store-assoc k (not v))
          nil)
        (error "store-swap (%s): '%s' is not a boolean" k v)))))

(defn store-rename-key
  "Renames <k1> to <k2> in the store"
  [k1 k2]
  (if-let [v (get @store k1)]
    (do
      ;; Avoid superfluous I/O
      (swap! store (fn [store] (assoc store k2 v)))
      (store-dissoc k1))
    (errorln "store-rename-key: No such key '%s'" k1)))

;; Storing API keys
(defn add-API-key
  "Puts an <API-key> named <name> in the store"
  [name API-key]
  (if (= API-key "FIXME_PUT_YOUR_API_KEY_HERE")
    (error "You have to replace \"FIXME_PUT_YOUR_API_KEY_HERE\" with the actual API key.")
    (store-assoc :API-keys (assoc (or (store-get :API-keys) {}) name API-key))))

(defn get-API-key?
  "Gets any API key named <name> in the store"
  [name]
  (get (store-get :API-keys) name))

(defn get-API-key
  "Gets any API key named <name> in the store. Errors if no such key exists"
  [key-name]
  (if-let [api-key (get-API-key? key-name)]
    api-key
    (error "Missing API key %s. Please add it via (add-API-key %s \"KEY-GOES-HERE\")"
           key-name key-name)))

(defn list-API-keys
  "Lists all of the API keys in the store"
  []
  (store-get :API-keys))

(def +cached-get-directions-data+ "cached get-directions data")
(def +cached-autocomplete-data+ "cached autocomplete data")

(defn store-invalidate-all-directions
  "Removes all cached directions data from the store"
  []
  (swap! store (fn [store] (into {} (filter #(or
                                          (not (map? (second %)))
                                          (not (get (second %) :type))
                                          (not= (get (second %) :type) +cached-get-directions-data+))
                                        (seq store)))))
  (write-store-to-file)
  store)

(defn store-invalidate-all-autocomplete-data
  "Removes all cached autocomplete data from the store"
  []
  (swap! store (fn [store] (into {} (filter #(or
                                          (not (map? (second %)))
                                          (not (get (second %) :type))
                                          (not= (get (second %) :type) +cached-autocomplete-data+))
                                        (seq store)))))
  (write-store-to-file)
  store)

(defn add-walk-data-point
  "Add a walk data point spanning <meters> meters over <seconds> seconds, identified by a unique <tag>"
  [tag meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                    {:tag tag
                                     :type :walk
                                     :time (current-timestamp)
                                     :meters meters
                                     :seconds (timestring-to-seconds? seconds)}))
  nil)

(defn add-jog-data-point
  "Adds a jog data point spanning <meters> meters over <seconds> seconds, identified by a unique <tag>"
  [tag meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                      {:tag tag
                                       :type :jog
                                       :time (current-timestamp)
                                       :meters meters
                                       :seconds (timestring-to-seconds? seconds)}))
  nil)

(defn add-run-data-point
  "Adds a run data point spanning <meters> meters over <seconds> seconds, identified by a unique <tag>"
  [tag meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                    {:tag tag
                                     :type :run
                                     :time (current-timestamp)
                                     :meters meters
                                     :seconds (timestring-to-seconds? seconds)}))
  nil)

(defn add-walk-data-point-with-note
  "Adds a walk data point spanning <meters> meters over <seconds> seconds, identified by a unique <tag> and <note>"
  [tag note meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                 {:tag tag
                                  :type :walk
                                  :note note
                                  :time (current-timestamp)
                                  :meters meters
                                  :seconds (timestring-to-seconds? seconds)})))

(defn add-jog-data-point-with-note
  "Adds a jog data point spanning <meters> meters over <seconds> seconds, identified by a unique <tag> and <note>"
  [tag note meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                 {:tag tag
                                  :type :jog
                                  :note note
                                  :time (current-timestamp)
                                  :meters meters
                                  :seconds (timestring-to-seconds? seconds)})))

(defn add-run-data-point-with-note
  "Adds a run data point spanning <meters> meters over <seconds> seconds, identified by a unique <tag> and <note>"
  [tag note meters seconds]
  (store-assoc :movement-data (conj (or (store-get :movement-data) [])
                                 {:tag tag
                                  :type :run
                                  :note note
                                  :time (current-timestamp)
                                  :meters meters
                                  :seconds (timestring-to-seconds? seconds)})))

(defn movement-data
  "Returns all movement data (walk, run, jog) contained in the store"
  []
  (map #(assoc % :meters-per-second (/ (:meters %) (:seconds %) 1.0)) (store-get :movement-data)))

(defn most-recent-movement-data
  "Returns the most recent movement data point contained in the store"
  []
  (first (movement-data)))

(defn redact
  "Anonymizes/redacts the notes and tags of a list (movement data, walk data, etc)"
  [li]
  (map #(assoc % :tag "[REDACTED]" :note "[REDACTED]") li))

(defn add-movement-data-note
  "Adds a note to the most recent movement data point in the store"
  [note]
  (if (seq (store-get :movement-data))
    (let [datum (first (store-get :movement-data))
          movement-data (rest (store-get :movement-data))]
      (store-assoc :movement-data (conj movement-data (assoc datum :note note)))
      (most-recent-movement-data))
    (error "add-movement-data-note: There is no stored movement data yet.")))

(defn remove-most-recent-movement-data
  "Removes the most recent movement data point in the store"
  []
  (store-assoc :movement-data (rest (store-get :movement-data)))
  (most-recent-movement-data))

(defn remove-movement-data-where-tag=
  "Removes any movement data where the tag is <tag>"
  [tag]
  (store-assoc :movement-data (filter #(not= (:tag %) tag) (movement-data))))

(defn update-movement-data-point-at-idx
  "Updates the <k> of the <idx>th movement data point in the store to <v>. 0 = most recent"
  [idx k v]
  (store-assoc :movement-data
               (seq (assoc (vec (movement-data)) idx (assoc (nth (movement-data) idx) k v)))))

(defn update-all-movement-data-points
  "Updates the <k> of every movement data point to <v>"
  [k v]
  (store-assoc :movement-data
               (vec (map #(assoc % k v) (movement-data)))))

(defn walk-data
  "Returns all the walk data in the store"
  []
  (filter #(= (:type %) :walk) (movement-data)))

(defn grep-walk-data
  "Returns all the walk data in the store that has a tag which includes <str>"
  [str]
  (filter #(str/includes? (:tag %) str) (walk-data)))

(defn jog-data
  "Returns all the jog data in the store"
  []
  (filter #(= (:type %) :jog) (movement-data)))

(defn run-data
  "Returns all the run data in the store"
  []
  (filter #(= (:type %) :run) (movement-data)))

(defn walk-data-idx
  "Returns all the walk data in the store, with indexes (for use in (update-movement-data-point-at-idx), etc)"
  []
  (map-indexed #(assoc %2 :idx %1) (walk-data)))

(defn jog-data-idx
  "Returns all the jog data in the store, with indexes (for use in (update-movement-data-point-at-idx), etc)"
  []
  (map-indexed #(assoc %2 :idx %1) (jog-data)))

(defn run-data-idx
  "Returns all the run data in the store, with indexes (for use in (update-movement-data-point-at-idx), etc)"
  []
  (map-indexed #(assoc %2 :idx %1) (run-data)))