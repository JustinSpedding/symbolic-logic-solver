(defproject symbolic-logic-solver "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.nrepl "0.2.12"]]
  :main ^:skip-aot symbolic-logic-solver.core
  :target-path "target/%s"
  :plugins [[refactor-nrepl "1.1.0"]
            [cider/cider-nrepl "0.13.0"]]
  :profiles {:uberjar {:aot :all}}
             :debug-repl {:resource-paths ["/usr/lib/jvm/java-8-openjdk-amd64/lib/tools.jar"]
                          :repl-options {:nrepl-middleware [debug-middleware.core/debug-middleware]}
                          :plugins [[refactor-nrepl "1.1.0"]
                                    [cider/cider-nrepl "0.13.0"]
                                    [venantius/ultra "0.4.1"]]
                          :dependencies [[org.clojure/clojure "1.8.0"]
                                         [debug-middleware "0.5.1"]
                                         [compliment "0.2.7"]
                                         [org.clojure/tools.nrepl "0.2.12"]]})
