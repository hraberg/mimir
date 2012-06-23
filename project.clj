(defproject mimir/mimir "0.1.0-SNAPSHOT"
  :description "Mímir is an experimental rule engine written in Clojure"
  :repositories {"sonatype snapshots"
                 "https://oss.sonatype.org/content/repositories/snapshots/"}
  :dependencies [[org.clojure/clojure "1.5.0-alpha2"]
                 [log4j/log4j "1.2.16"]
                 [org.clojure/tools.logging "0.2.3"]]
  :profiles {:dev {:dependencies [[marginalia "0.7.1"]]}}
  :plugins [[lein-difftest "1.3.8"]]
  :repl-init mimir.well
  :aot [mimir.well]
  :main mimir.well
  :min-lein-version "2.0.0")
