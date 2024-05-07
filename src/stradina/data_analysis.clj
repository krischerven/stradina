(ns stradina.data-analysis
  (:gen-class)
  (:require [stradina.math :as math])
  (:use [stradina.store]
        [stradina.util]))

(def ^:dynamic *walk-radius* (math/minutes 10))

(defmacro with-walk-radius-of [walk-radius & body]
  `(binding [*walk-radius* ~walk-radius]
     ~@body))

(defn my-average-movement-speed []
  (if (seq (movement-data))
    (math/mean (map #(/ (:meters %) (:seconds %) 1.0)
                    (filter #(not (:ignore %)) (movement-data))))
    math/typical-walk-speed))

(defn my-average-walk-speed []
  (if (seq (walk-data))
    (math/mean (map #(/ (:meters %) (:seconds %) 1.0)
                    (filter #(not (:ignore %)) (walk-data))))
    math/typical-walk-speed))

(defn my-average-jog-speed []
  (if (seq (jog-data))
    (math/mean (map #(/ (:meters %) (:seconds %) 1.0)
                    (filter #(not (:ignore %)) (jog-data))))
    (if (seq (walk-data))
      ;; Untested formula - assume that for every N% faster a person walks, they jog sqrt(N)% faster
      (* (Math/sqrt (/ (my-average-walk-speed) math/typical-walk-speed)) math/typical-jog-speed)
      math/typical-jog-speed)))

(defn my-average-run-speed []
  (if (seq (run-data))
    (math/mean (map #(/ (:meters %) (:seconds %) 1.0)
                    (filter #(not (:ignore %)) (run-data))))
    (if (seq (walk-data))
      ;; Untested formula - assume that for every N% faster a person walks, they run sqrt(N)% faster
      (* (Math/sqrt (/ (my-average-walk-speed) math/typical-walk-speed)) math/typical-run-speed)
      math/typical-run-speed)))

(defn my-average-walk-time []
  (if (seq (walk-data))
    (math/mean (map #(/ (:seconds %) 60 1.0)
                    (filter #(not (:ignore %)) (walk-data))))
    ##NaN))

(defn my-average-jog-time []
  (if (seq (jog-data))
    (math/mean (map #(/ (:seconds %) 60 1.0)
                    (filter #(not (:ignore %)) (jog-data))))
    ##NaN))

(defn my-average-run-time []
  (if (seq (run-data))
    (math/mean (map #(/ (:seconds %) 60 1.0)
                    (filter #(not (:ignore %)) (run-data))))
    ##NaN))

(defn calculate-rectangle-acreage [time-across time-lengthwise]
  (* 640
     (/ (* time-across (my-average-walk-speed)) math/meters-per-mile)
     (/ (* time-lengthwise (my-average-walk-speed)) math/meters-per-mile)))

(defn store-acreage-data [name time-across time-lengthwise]
  (store-assoc :acreage-data (conj (or (store-get :acreage-data) [])
                                   {:name name
                                    :time-across time-across
                                    :time-lengthwise time-lengthwise
                                    :acreage (calculate-rectangle-acreage time-across time-lengthwise)})))

;; Walk radii: How far you can walk in *walk-radius* minutes
(defn walk-radius [speed-const]
  (formatln "Walk radius of %f miles" (/ (* speed-const *walk-radius*) math/meters-per-mile)))

(defn google-walk-radius [] (walk-radius math/google-maps-approx-walk-speed))
(defn slow-walk-radius [] (walk-radius math/slow-walk-speed))
(defn leisurely-walk-radius [] (walk-radius math/leisurely-walk-speed))
(defn typical-walk-radius [] (walk-radius math/typical-walk-speed))
(defn fastish-walk-radius [] (walk-radius math/fastish-walk-speed))
(defn brisk-walk-radius [] (walk-radius math/brisk-walk-speed))
(defn jog-radius [] (walk-radius math/typical-jog-speed))
(defn run-radius [] (walk-radius math/typical-run-speed))
(defn my-walk-radius [] (walk-radius (my-average-walk-speed)))

;; Walkshed: The circle extending outward *walk-radius* minutes in all directions
(defn walkshed [speed-const]
  (formatln "Walkshed of %f square miles" (/ (* Math/PI (Math/pow (* speed-const *walk-radius*) 2))
                                             math/meters-per-square-mile)))

(defn google-walkshed [] (walkshed math/google-maps-approx-walk-speed))
(defn slow-walkshed [] (walkshed math/slow-walk-speed))
(defn leisurely-walkshed [] (walkshed math/leisurely-walk-speed))
(defn typical-walkshed [] (walkshed math/typical-walk-speed))
(defn fastish-walkshed [] (walkshed math/fastish-walk-speed))
(defn brisk-walkshed [] (walkshed math/brisk-walk-speed))
(defn jogshed [] (walkshed math/typical-jog-speed))
(defn runshed [] (walkshed math/typical-run-speed))
(defn my-walkshed [] (walkshed (my-average-walk-speed)))