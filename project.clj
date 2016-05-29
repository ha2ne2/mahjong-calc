(defproject mj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :plugins [[lein-cljsbuild "1.1.3"]]
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 ;[criterium "0.4.3"]
                ;[org.clojure/test.check "0.9.0"]
                 [clj-http "2.0.1"]
                 [org.clojure/clojurescript "1.8.51"]]
  :main ^:skip-aot mj.core
  :target-path "target/%s"
  :profiles {:resource-paths ["dummy-data"]
             :uberjar {:aot :all}}
  :cljsbuild {
              :builds [{
        ; The path to the top-level ClojureScript source directory:
        :source-paths ["src"]
        ; The standard ClojureScript compiler options:
        ; (See the ClojureScript compiler documentation for details.)
        :compiler {
          :output-to "main.js"  ; default: target/cljsbuild-main.js
          :optimizations :whitespace
          :pretty-print true}}]}
  )
