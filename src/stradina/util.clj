(ns stradina.util
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [apropos]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test :refer [deftest]]
            [clojure.edn :as edn]))

(defn formatln
  "Formats a string using format and prints it to the console."
  [msg & args]
  (println (apply format msg args)))

(defn color-println [message color]
  (let [colors {:red "\u001b[31m"
                :green "\u001b[32m"
                :yellow "\u001b[33m"
                :blue "\u001b[34m"
                :reset "\u001b[0m"}]
    (print (get colors color))
    (print message)
    (print (get colors :reset))
    (println)))

(defn here []
  (color-println "DEBUG: HERE" :red))

(defn noticeln [msg & args]
  (color-println (apply format (format "Notice: %s" msg) args) :blue))

(defn warnln [msg & args]
  (color-println (apply format (format "Warning: %s" msg) args) :red))

(defn errorln [msg & args]
  (color-println (apply format (format "Error: %s" msg) args) :red))

(defn =one [x & y]
  (loop [y y]
    (if (seq y)
      (if (= x (first y))
        true
        (recur (rest y)))
      false)))

(defn yesno
  "Ask the caller a question and ask them for a y(es)/n(no) answer.
   Return true on a yes answer."
  [question & args]
  (apply formatln (str question  " (y/n)") args)
  (=one (str/lower-case (read-line)) "y" "yes"))

(defn get-input [command]
  (println command)
  (read-line))

(defn error [err & fmt]
  (throw (RuntimeException. (apply format err fmt))))

(defn replace-all [str patterns]
  (loop [str str patterns patterns]
    (if (seq patterns)
      (recur (str/replace str (first patterns) "") (rest patterns))
      str)))

(defn strip-html [text]
  (replace-all text (list "<b>" "</b>" "<wbr/>" "<div style=\"font-size:0.9em\">" "<div>" "</div>")))

(defn string-replace-multi [text replacement-pairs]
  (loop [text text pairs replacement-pairs]
    (if (seq pairs)
      (let [pair (first pairs)]
        (recur (str/replace text (first pair) (second pair)) (rest pairs)))
      text)))

(defn current-timestamp []
  (let [timestamp (java.time.LocalDateTime/now)
        formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format formatter timestamp)))

(defn pwd []
  (System/getProperty "user.dir"))

(defn file-exists? [file-path]
  (.exists (io/file file-path)))

(defn delete-file? [file-path]
  (when (file-exists? file-path)
    (io/delete-file file-path)))

(defn read-map-from-file [file-path]
  (with-open [reader (io/reader file-path)]
    (or (edn/read-string (slurp reader)) {})))

(defn read-map-from-file? [file-path]
  (if (file-exists? file-path) (read-map-from-file file-path) {}))

(defn write-map-to-file [map file-path]
  (with-open [writer (io/writer file-path)]
    (binding [*out* writer]
      (print (pr-str map)))))

(defmacro niling [& body]
  `(do ~@body nil))

(defn map-keys [f m]
  (zipmap (map f (keys m)) (vals m)))

(defn timestring-to-seconds
  "Convert a string like '1534' into 934 (seconds)"
  [timestring]
  (assert (>= (count timestring) 3))
  (let [secs (Integer/parseInt (subs timestring (- (count timestring) 2)))
        mins (Integer/parseInt (subs timestring 0 (- (count timestring) 2)))]
    (+ secs (* mins 60))))

(defn timestring-to-seconds?
  "If <timestring> is a string, then return (timestring-to-seconds <timestring>).
  Otherwise, return <timestring>."
  [timestring]
  (if (string? timestring)
    (timestring-to-seconds timestring)
    timestring))

(deftest test-timestring-to-seconds []
  (test/is (= (timestring-to-seconds "1926") 1166))
  (test/is (= (timestring-to-seconds "1834") 1114))
  (test/is (= (timestring-to-seconds "1537") 937))
  (test/is (= (timestring-to-seconds "909") 549)))