(defproject pp-code "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [nstools "0.2.4"]
                 [anglican "1.0.0"]
                 [gorilla-plot "0.1.4"]]
  :plugins [[dtolpin/lein-gorilla "0.3.7-SNAPSHOT"]]
  :resource-paths ["programs"]
  :main ^:skip-aot pp-code.SimpleCrossing)
