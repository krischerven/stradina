(defproject stradina "0.1.0"
  :description "An interactive research program about walking and urban form."
  :url "https://git.krischerven.info/dev/stradina"
  :license {:name "AGPL-3.0-or-later"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html#license-text"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/data.json "2.5.0"]
                 [org.clojure/core.cache "0.7.1"]
                 [clj-http "3.11.0"]]
  :main ^:skip-aot stradina.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
