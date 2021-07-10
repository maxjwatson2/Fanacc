(defproject fintwo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.10.866"]
                 [metosin/compojure-api "2.0.0-alpha30"]
                 [compojure "1.1.6"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-core "1.9.1"]
                 [ring/ring-jetty-adapter "1.9.1"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-mock "0.4.0"]
                 [hiccup "1.0.5"]
                 [reagent "1.1.0"]
                 [reagent-utils "0.3.3"]
                 [cljsjs/react "17.0.2-0"]
                 [cljsjs/react-dom "17.0.2-0"]
                 ;   [reagent "1.0.0"]
                 [cheshire "5.10.0"]


                 ]
  :main ^:skip-aot fintwo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
