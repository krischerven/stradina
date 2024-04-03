(ns stradina.core
  (:gen-class)
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [stradina.math :as math])
  (:use [stradina.data-analysis]
        [stradina.util]
        [stradina.store]
        [stradina.cache]))

(defonce ^:dynamic *invalidating-cache* false)
(defmacro invalidating-cache [& body]
  `(binding [*invalidating-cache* true]
     ~@body))

(defonce ^:dynamic *ignoring-city* false)
(defmacro ignoring-city [& body]
  `(binding [*ignoring-city* true]
     ~@body))

(defn --get-directions
  "Get the directions from origin to destination using the Google Directions API.
  Do not call this function directly from the REPL; instead, use print-directions."
  [origin destination]

  (when-let [continue (if-not (store-get :city)
                        (do
                          (warnln (str "There is no :city specified in the store. "
                                       "This may lead to poor directions or API errors/overuse.\n"
                                       "Please run (store-assoc :city \"YOUR-CITY-NAME-HERE\") "
                                       "to suppress this warning."))
                          (yesno "Continue anyway?"))
                        true)]

    (let [url (str "https://maps.googleapis.com/maps/api/directions/json"
                   "?origin=" origin
                   "&destination=" destination
                   "&mode=walking"
                   "&key=" (get-API-key :maps))
          cache-key url
          cache-val (if *invalidating-cache*
                      (do
                        (cache-dissoc cache-key)
                        nil)
                      (cache-get cache-key))]

      (when cache-val
        (when (store-get :print-cache-messages)
          (formatln (str "Using cached directions (as of %s). "
                         "To invalidate this cache, run '(cache-dissoc \"%s\")'\n")
                    (:date cache-val) cache-key))
        (assert (and (:date cache-val) (:data cache-val))))

      (let [response (if cache-val nil (client/get url))
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
              (str (string-replace-multi dirs [["Destination will be on the"
                                                "\nYour destination will be on the"]
                                               ["Take the stairs"
                                                "\nTake the stairs"]
                                               ["Pass by"
                                                "\nPass by"]
                                               ["will be on the right" "will be on the right."]
                                               ["will be on the left" "will be on the left."]])
                   "\nThis will be a " distance " (" distance-v " meter)" " walk taking up to "
                   (str/replace duration "mins" "minutes") ",\n"
                   "but probably ~" (math/movement-speed distance-v (my-average-walk-speed))
                   " minutes (your personalized estimate),\n"
                   "or a ~" (math/movement-speed distance-v (my-average-jog-speed)) " minute jog,\n"
                   "or a ~" (math/movement-speed distance-v (my-average-run-speed)) " minute run."))))))))

(defn with-city-suffix [placename]
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
                 (-> (client/get url {:query-params params}) :body (json/read-str :key-fn keyword)))]
    (when cache-val
      (when (store-get :print-cache-messages)
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

(defn replace-aliases [placename]
  (let [l-placename (str/lower-case placename)]
    (cond (=one l-placename "home" "old home")

          ;; "home", :home, or "old home" [But see (keyword "old home")]
          (or (store-get l-placename) (store-get (keyword l-placename)) placename)

          (apply =one l-placename (map str/lower-case (keys (or (store-get :aliases) {}))))
          (get (map-keys str/lower-case (store-get :aliases)) l-placename)

          :else placename)))

(defn print-directions
  "Print the directions from origin to destination using the Google Directions API."
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
                (println (--get-directions origin destination)))))

      (let [origin (autocomplete-place origin)
            destination (autocomplete-place destination)]
        (println (--get-directions origin destination))))))

(defn find-path []
  (let [str (read-line)
        split (str/split str #" to ")]
    (if (= (count split) 2)
      (print-directions (first split) (second split))
      (error "Wrong format: Should be '[loc1] to [loc2]'"))))

(defn doctor []
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