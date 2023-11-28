(defproject snake "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :main ^:skip-aot snake.coDre
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [quil "4.3.1323"]
                 [org.craigandera/dynne "0.4.1"]]
  :profiles {:uberjar {:aot :all}})