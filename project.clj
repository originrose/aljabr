(defproject thinktopic/aljabr "0.1.0"
  :description  "A core.matrix implementation for Clojure and Clojurescript"
  :url          "http://github/thinktopic/aljabr"
  :license      {:name "Apache Software License 2.0"
                 :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [net.mikera/core.matrix "0.50.0"]
                 [thi.ng/typedarrays "0.1.2"]]

  :plugins      [[lein-cljsbuild "1.0.6"]
                 [lein-figwheel "0.5.0-4"]]

  :figwheel {:server-port 2345}

  :source-paths ["src" "test"]

  :cljsbuild {:builds [{:id :dev
                        :figwheel true
                        :source-paths ["src"]
                        :compiler {:output-to "resources/public/js/thinktopic.aljabr.js"
                                   :output-dir "resources/public/js/out"
                                   :asset-path "js/out"
                                   :optimizations :none
                                   :pretty-print true}}
                       {:id :test
                        :figwheel true
                        :source-paths ["src" "test"]
                        :compiler {:output-to "resources/public/js/thinktopic.aljabr.test.js"
                                   :output-dir "resources/public/js/test-out"
                                   :asset-path "js/test-out"
                                   :main "thinktopic.aljabr.test-all"
                                   :optimizations :none
                                   :pretty-print true}}
                       ]

              :test-commands {"unit-tests" ["open" :runner "test.html"]}}

  :deploy-repositories [["snapshots" :clojars]
                        ["releases" :clojars]]

  :min-lein-vesion "2.5.0")
