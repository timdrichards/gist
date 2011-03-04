(defproject gist "0.0.1"
            :description "An automatic code generator generator"
            :dependencies [[org.clojure/clojure "1.2.0"]
                           [org.clojure/clojure-contrib "1.2.0"]
                           [midje "1.1-alpha-1"]
                           [matchure "0.10.1"]
                           [ring/ring-core "0.3.6"]
                           [ring/ring-devel "0.3.6"]
                           [ring/ring-jetty-adapter "0.3.6"]
                           [compojure "0.6.0"]
                           [hiccup "0.2.6"]]
            :dev-dependencies [[swank-clojure "1.2.0"]
                               [lein-ring "0.3.2"]]
            :repositories {"clojure-releases"       "http://build.clojure.org/releases"
                           "clojars.org"            "http://clojars.org/repo"}
            :source-path "src"
            ;  :aot  [gist.tool]
            ;  :main gist.tool
            :ring {:handler wist.core/app}
            )
