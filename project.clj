(defproject symbolic-logic-solver "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot symbolic-logic-solver.core
  :target-path "target/%s"
  :plugins [[refactor-nrepl "1.1.0"]
            [cider/cider-nrepl "0.9.1"]]
  :profiles {:uberjar {:aot :all}})
