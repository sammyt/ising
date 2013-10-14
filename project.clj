(defproject retro "0.1.0-SNAPSHOT"
  :description "redoing my degree - in a browser, sorta"
  :license {:name "MIT"
            :url "http://sammyt.mit-license.org"}
  :repositories {"sonatype-staging" "https://oss.sonatype.org/content/groups/staging"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1913"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]
                 [com.keminglabs/c2 "0.2.3"]]
  :hooks [leiningen.cljsbuild]
  :plugins [[lein-cljsbuild "0.3.3"]
            [lein-kibit "0.0.8"]]
  :cljsbuild {
     :builds [{
       :id "dev"
       :source-paths ["src/retro"]
       :compiler {
          :output-to "public/retro-dev.js"
          :pretty-print true }} ]})
