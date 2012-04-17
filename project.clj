(defproject mimir "0.1.0-SNAPSHOT"
  :description "Mímir is an experimental rule engine written in Clojure"
  :dependencies [[clojure "1.4.0"]
                 [log4j/log4j "1.2.16"]
                 [org.clojure/tools.logging "0.2.3"]]
  :dev-dependencies [[marginalia "0.7.0"]
                     [lein-difftest "1.3.7"]]
  :repl-init mimir.well
  :main mimir.well
  :aot [mimir.well])
