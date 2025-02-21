(ns stradina.core
  (:gen-class)
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [stradina.math :as math])
  (:use [clojure.repl]
        [stradina.AI]
        [stradina.data-analysis]
        [stradina.util]
        [stradina.store]
        [stradina.cache]))

(defonce ^:dynamic *invalidating-cache* false)
(defmacro invalidating-cache
  "Invalidates cached directions within a scope"
  [& body]
  `(binding [*invalidating-cache* true]
     ~@body))

(defonce ^:dynamic *ignoring-city* false)
(defmacro ignoring-city
  "Ignores the city within a scope"
  [& body]
  `(binding [*ignoring-city* true]
     ~@body))

(defonce ^:dynamic *cache-messages-on* true)
(defmacro without-cache-messages
  "Suppresses cache messages within a scope"
  [& body]
  `(binding [*cache-messages-on* false]
     ~@body))

(defonce ^:dynamic *internet-access-enabled* true)
(defmacro without-internet-access
  "Disables internet access within a scope"
  [& body]
  `(binding [*internet-access-enabled* false]
     ~@body))

(defonce ^:dynamic *get-directions-mode* "walking")
(defmacro driving
  "Dynamically rebinds *get-directions-mode* to \"driving\" within a scope"
  [& body]
  `(binding [*get-directions-mode* "driving"]
     ~@body))

(defmacro biking
  "Dynamically rebinds *get-directions-mode* to \"biking\" within a scope"
  [& body]
  `(binding [*get-directions-mode* "bicycling"]
     ~@body))

(defmacro transiting
  "Dynamically rebinds *get-directions-mode* to \"transiting\" within a scope"
  [& body]
  `(binding [*get-directions-mode* "transit"]
     ~@body))

(defn walking?
  "Returns true if the current movement mode is walking, else returns false"
  []
  (= *get-directions-mode* "walking"))

(defn movement-type
  "Dynamically rebinds *get-directions-mode* to \"transiting\" within a scope"
  []
  (cond (= *get-directions-mode* "walking") "walk"
        (= *get-directions-mode* "driving") "drive"
        (= *get-directions-mode* "bicycling") "cycling"
        (= *get-directions-mode* "transit") "transit trip"))

(defn cache-messages
  "Returns true if cache-messages are logged, else returns false"
  []
  (and (store-get :print-cache-messages) *cache-messages-on*))

(defn --get-directions-get-url-and-cache-kv
  "Does what it says on the tin; only invoked by (--get-directions) and (print-directions-cache)"
  [origin destination]
  (let [url (str "https://maps.googleapis.com/maps/api/directions/json"
                 "?origin=" origin
                 "&destination=" destination
                 "&mode=" *get-directions-mode*
                 "&key=" (get-API-key :maps))
        cache-key url]
    {:url url
     :cache-key cache-key
     :cache-val (if *invalidating-cache*
                  (do
                    (cache-dissoc cache-key)
                    nil)
                  (cache-get cache-key))}))

(defn client-get
  "A wrapper around client/get that only works when internet access is enabled"
  [url & [req & r]]
  (if *internet-access-enabled*
    (apply client/get url req r)
    (error "client-get called when internet access is disabled")))

(defn --get-directions
  "Get the directions from origin to destination using the Google Directions API.
  Do not call this function directly from the REPL; instead, use print-directions"
  [origin destination]

  (when-let [continue (if-not (store-get :city)
                        (do
                          (warnln (str "There is no :city specified in the store. "
                                       "This may lead to poor directions or API errors/overuse.\n"
                                       "Please run (store-assoc :city \"YOUR-CITY-NAME-HERE\") "
                                       "to suppress this warning."))
                          (yesno "Continue anyway?"))
                        true)]

    (let [{url :url
           cache-key :cache-key
           cache-val :cache-val}
          (--get-directions-get-url-and-cache-kv origin destination)]

      (when cache-val
        (if (cache-messages)
          (formatln (str "Using cached directions (as of %s). "
                         "To invalidate this cache, run '(cache-dissoc \"%s\")'\n")
                    (:date cache-val) cache-key)
          (when *cache-messages-on*
            (println "(Using cached data)")))
        (assert (and (:date cache-val) (:data cache-val))))

      (let [response (if cache-val nil (client-get url))
            data (or (:data cache-val) (json/read-str (:body response) :key-fn keyword))]

        (cache-assoc? cache-key {:date (current-timestamp) :type +cached-get-directions-data+ :data data})

        (def |get-directions-data| data)

        (let [distance (-> data :routes first :legs first :distance :text)
              distance-v (-> data :routes first :legs first :distance :value)
              duration (-> data :routes first :legs first :duration :text)
              duration-v (-> data :routes first :legs first :duration :value)]

          (def |distance-v| distance-v)
          (def |duration-v| duration-v)

          (loop [dirs "" steps (-> data :routes first :legs first :steps)]
            (if (seq steps)
              (recur (str dirs (strip-html (:html_instructions (first steps))) "\n") (rest steps))
                ;; Maps API quirk
              {:directions (str (string-replace-multi dirs [["Destination will be on the"
                                                             "\nYour destination will be on the"]
                                                            ["Take the stairs"
                                                             "\nTake the stairs"]
                                                            ["Pass by"
                                                             "\nPass by"]
                                                            ["will be on the right" "will be on the right."]
                                                            ["will be on the left" "will be on the left."]])
                                "\nThis will be a " distance " (" distance-v " meter)" " " (movement-type)
                                " taking up to "
                                (str/replace duration "mins" "minutes")
                                (if (walking?) ",\n" ".")
                                (if (walking?) (str
                                                "but probably ~" (math/movement-speed distance-v (my-average-walk-speed))
                                                " minutes (your personalized estimate)"))
                                (if (walking?) ",\n")
                                (if (walking?) (str "or a ~" (math/movement-speed distance-v (my-average-jog-speed))
                                    " minute jog,\n"))
                                (if (walking?) (str "or a ~" (math/movement-speed distance-v (my-average-run-speed))
                                    " minute run.")))

               :distance distance-v})))))))

(defn with-city-suffix
  "Returns the provided <placename> string with the current city as a suffix.

  ;; For example:
  (with-store-assoc :city \"Detroit, MI\"
    (with-city-suffix \"DIA\")) ;; => \"DIA, Detroit, MI\"
  "

  [placename]
  (if *ignoring-city*
    placename
    (if-let [city (store-get :city)]
      (str placename ", " city)
      placename)))

(defn autocomplete-place
  "Autocomplete a place using the Google Places API."
  [placename]
  (let [api-key (get-API-key :maps)
        url "https://maps.googleapis.com/maps/api/place/autocomplete/json"
        params {:key api-key
                :input (with-city-suffix placename)}
        cache-key (str url "&" params)
        cache-val (if *invalidating-cache*
                    (do
                      (cache-dissoc cache-key)
                      nil)
                    (cache-get cache-key))
        data (or (:data cache-val)
                 (-> (client-get url {:query-params params}) :body (json/read-str :key-fn keyword)))]
    (when cache-val
      (when (cache-messages)
        (formatln (str "Using cached autocomplete data (as of %s). "
                       "To invalidate this cache, run '(cache-dissoc \"%s\")'\n")
                  (:date cache-val) cache-key))
      (assert (and (:date cache-val) (:data cache-val))))
    (cache-assoc? cache-key {:date (current-timestamp) :type +cached-autocomplete-data+ :data data})
    (def |autocomplete-place-data| data)
    (when-let [err (:error_message data)]
      (errorln "An error occured in autocomplete-place: \"%s\"" err))
    (when (= (:status data) "ZERO_RESULTS")
      (errorln "No results in autocomplete-place for placename \"%s\"" placename))
    (:description (first (:predictions data)))))

(defn replace-aliases
  "Replace the aliases \"home\" or \"old home\" IF they are contained in the store,
  as well as any aliases contained within (store-get :aliases)"
  [placename]
  (let [l-placename (str/lower-case placename)]
    (cond (=one l-placename "home" "old home")

          ;; "home", :home, or "old home" [But see (keyword "old home")]
          (or (store-get l-placename) (store-get (keyword l-placename)) placename)

          (apply =one l-placename (map str/lower-case (keys (or (store-get :aliases) {}))))
          (get (map-keys str/lower-case (store-get :aliases)) l-placename)

          :else placename)))

(defn correct-origin-destination
  "Use heuristics and the Google Places API to correct origin and destination, or fail if either is too ambiguous."
  [origin destination]
  (if (= (str/lower-case origin) (str/lower-case destination))
    (errorln "Origin and destination are the same.")

    (let [origin (replace-aliases origin)
          destination (replace-aliases destination)]

      (if (and (not (store-get :allow-ambiguous-places))
               (not (store-get :city)))

        ;; Prevent dangerous API overuse - "Whole Foods" can give you a destination hundreds of miles away,
        ;; with several hundred lines of directions.
        (cond (<= (count (str/split origin #" ")) 2)
              (warnln (str "The origin \"%s\" is too ambiguous to continue without a valid :city parameter in the store.\n"
                           "Please run (store-assoc :city \"YOUR-CITY-NAME-HERE\") to fix this warning.") origin)

              (<= (count (str/split destination #" ")) 2)
              (warnln (str "The destination \"%s\" is too ambiguous to continue without a valid :city parameter in the store.\n"
                           "Please run (store-assoc :city \"YOUR-CITY-NAME-HERE\") to fix this warning.") destination)

              :else
              (let [origin (autocomplete-place origin)
                    destination (autocomplete-place destination)]
                (list origin destination))))

      (let [origin (autocomplete-place origin)
            destination (autocomplete-place destination)]

        (list origin destination)))))

(defn get-directions
  "Run (--get-directions) with corrected origin/destination."
  [origin destination]
  (apply --get-directions (correct-origin-destination origin destination)))

(defn print-directions
  "Print the directions from origin to destination using the Google Directions API."
  [origin destination]
  (store-assoc :directions-history
               (conj (or (store-get :directions-history) [])
                     (format "(print-directions \"%s\" \"%s\")" origin destination)))
  (println (:directions (get-directions origin destination))))

(defn print-directions-cache
  "Print any cached directions from origin to destination and return nil"
  [origin destination]
  (without-cache-messages
   (let [[origin destination] (correct-origin-destination origin destination)]
     (when (:cache-val (--get-directions-get-url-and-cache-kv origin destination))
       (println (:directions (--get-directions origin destination)))))))

(defn to
  "An alias for (print-directions \"home\" \"$1\")"
  [destination]
  (if (store-get :home)
    (print-directions "home" destination)
    (errorln "No :home in store; aborting")))

(defn directions-history
  "Print the last 10 items in direction history"
  []
  (run! println (reverse (take-last 10 (store-get :directions-history))))
  (when (> (count (store-get :directions-history)) 10)
    (println "Truncating older items...")))

(defn add-walk-data-point-from-directions [origin destination time]
  (let [distance (:distance (get-directions origin destination))]
    (add-walk-data-point (str origin " to " destination) distance time)
    (first (walk-data))))

(defn add-jog-data-point-from-directions [origin destination time]
  (let [distance (:distance (get-directions origin destination))]
    (add-jog-data-point (str origin " to " destination) distance time)
    (first (jog-data))))

(defn add-run-data-point-from-directions [origin destination time]
  (let [distance (:distance (get-directions origin destination))]
    (add-run-data-point (str origin " to " destination) distance time)
    (first (run-data))))

(defn finalize-pending-movement-data [data]
  (doseq [datum data]
    (let [[origin destination time] datum]
      (when (yesno (str "Found datum {:origin %s :destination %s :time %s}. "
                        "Finalize this datum?") origin destination time)

        (let [origin (if (yesno "Is origin '%s' correct?" origin)
                       origin
                       (get-input "Enter the correct origin:"))

              destination (if (yesno "Is destination '%s' correct?" destination)
                            destination
                            (get-input "Enter the correct destination:"))

              time (if (yesno "Is time '%s' correct?" time)
                     time
                     (get-input "Enter the correct time:"))]

          (add-walk-data-point-from-directions origin destination time))))))

(defn pending-walk-data []
  (store-get :pending-walk-data))

(defn add-pending-walk-data-point [origin destination time]
  (store-assoc :pending-walk-data
               (conj (pending-walk-data) [origin destination time])))

(defn finalize-pending-walk-data []
  (finalize-pending-movement-data (pending-walk-data)))

(defn pending-jog-data []
  (store-get :pending-jog-data))

(defn add-pending-jog-data-point [origin destination time]
  (store-assoc :pending-jog-data
               (conj (pending-jog-data) [origin destination time])))

(defn finalize-pending-jog-data []
  (finalize-pending-movement-data (pending-jog-data)))

(defn pending-run-data []
  (store-get :pending-run-data))

(defn add-pending-run-data-point [origin destination time]
  (store-assoc :pending-run-data
               (conj (pending-run-data) [origin destination time])))

(defn finalize-pending-run-data []
  (finalize-pending-movement-data (pending-run-data)))

(defn find-path
  "Interactively prompts the user for a string in the format of 'LOC1 to LOC2', then
runs (print-directions <LOC1> <LOC2>)"
  []
  (let [str (read-line)
        split (str/split str #" to ")]
    (if (= (count split) 2)
      (print-directions (first split) (second split))
      (error "Wrong format: Should be '[loc1] to [loc2]'"))))

(defn doctor
  "Debugs trivially fixable problems with your Stradina configuration."
  []
  (println "The doctor will see you now...")
  (let [problems (atom 0)]
    (letfn [(problem [severity desc]
              (case severity
                :error (errorln desc)
                :warning (warnln desc)
                :notice (noticeln desc)
                (error "doctor/problem: Missing severity '%s'" severity))
              (swap! problems inc))]
      (when-not (get-API-key? :maps)
        (problem :error "Missing Google Maps API key. Please run (add-API-key :maps \"YOUR_API_KEY_HERE\")"))
      ;; See https://learn.microsoft.com/en-us/purview/sit-defn-google-api-key
      (when-let [key (get-API-key? :maps)]
        (when (or (not (str/starts-with? key "AIza"))
                  (not (= 39 (count key))))
          (problem :error "Google Maps API key is malformed. Please run (add-API-key :maps \"YOUR_NEW_API_KEY_HERE\")")))
      (when-not (store-get :city)
        (problem :warning "No :city in store. Please run (store-assoc :city \"YOUR_CITY_NAME_HERE\")"))
      (when (and (not (store-get "home"))
                 (not (store-get :home)))
        (problem :notice (str "No :home in store. This may be inconvenient.\n"
                              " - You can fix this issue by running (store-assoc :home \"YOUR_ADDRESS_HERE\")\n"
                              " - This will allow you to use \"home\" (without the quotes) in place of your address.")))
      (when (= @problems 0)
        (println "No problems detected!")))))

;; Non-interactive usage (Not recommended)
(defn -main
  [& args]
  (println "Usage: [loc1] to [loc2]")
  (find-path))