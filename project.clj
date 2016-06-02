(defproject jp "1.3.0"
  :description "An ode to jutils."
  :url "http://github.com/lgastako"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild                 "1.1.3"]
            [lein-doo                       "0.1.6"]]
  :dependencies [[its-log                   "3.0.0"]
                 [org.clojure/clojure       "1.8.0"]
                 [org.clojure/clojurescript "1.8.40"]]
  :cljsbuild {:builds {:test {:source-paths ["src" "test"]
                              :compiler {:optimizations :whitespace
                                         :output-to "out/jp-test.js"
                                         :main jp.runner
                                         :pretty-print true}}}})
