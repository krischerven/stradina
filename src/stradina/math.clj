(ns stradina.math
  (:gen-class))

(def google-straight-shot 1.214285714285714) ;; approx
(def google-home-to-redacted8 1.178301093355761) ;; approx

(def google-maps-approx-walk-speed 1.2)

(def slow-walk-speed 1.2)

(def leisurely-walk-speed 1.3)

;; https://www.runnersworld.com/uk/health/a45709070/average-walking-speed/
(def typical-walk-speed 1.4)

(def fastish-walk-speed 1.5)

(def brisk-walk-speed 1.8)

;; https://www.healthline.com/health/average-jogging-speed#at-a-jog
(def typical-jog-speed 2.2)

;; https://www.omnicalculator.com/sports/pace
(def typical-run-speed 3.35)

(def meters-per-mile 1609.34)

(def meters-per-square-mile 2.59e+6)

(defn meters-to-feet [meters] (* meters 3.28084))
(defn feet-to-meters [feet] (/ feet 3.28084))
(defn movement-speed [meters speed-const] (Math/round (/ meters speed-const 60 1.0)))

(defn mean [coll]
  (/ (reduce + coll) (count coll)))

(defn minutes [n]
  (* 60 n))