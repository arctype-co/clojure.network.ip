(defproject kovacnica/clojure.network.ip "0.1.4-SNAPSHOT"
  :description "Clojure/Clojurescript library for IP address and Network operations."
  :target-path "target/%s/"
  :compile-path "%s/classy-files"
  :url "https://github.com/kovacnica/clojure.networking"
  :source-paths ["src"]
  :test-paths ["test"]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.0"]
                 [org.clojure/clojurescript "1.11.4"]]
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :cljsbuild {:builds [{:id "network"
                        :source-paths ["src"]
                        :figwheel true
                        :compiler {:main "clojure.network.ip"
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/network.js"
                                   :output-dir "resources/public/js/out"}}]})
