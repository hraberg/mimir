(defproject mimir/mimir "0.1.0-SNAPSHOT"
  :description "Mímir is an experimental rule engine written in Clojure"
  :repositories {"sonatype snapshots"
                 "https://oss.sonatype.org/content/repositories/snapshots/"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.flatland/ordered "1.5.1"]
                 [log4j/log4j "1.2.16"]
                 [org.clojure/tools.logging "0.2.3"]]
  :profiles {:dev {:dependencies [[marginalia "0.7.1"]
                                  [clojure-lanterna "0.9.2"]
                                  [org.clojure/tools.trace "0.7.5"]]}}
  :plugins [[lein-swank "1.4.5"]
            [lein-difftest "2.0.0"]]
  :test-selectors {:default (complement :mk) :mk :mk}
  :repl-init mimir.well
  :main mimir.well
  :min-lein-version "2.0.0")
