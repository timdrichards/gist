(defproject gist "0.0.1"
  :description "An automatic code generator generator"
  :dependencies [[org.clojure/clojure "1.3.0-alpha4"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [com.stuartsierra/lazytest "2.0.0-SNAPSHOT"]
                 [matchure "0.10.1"]]
  :dev-dependencies [[swank-clojure "1.2.0"]]
  :repositories {"clojure-releases"       "http://build.clojure.org/releases"
                 "clojars.org"            "http://clojars.org/repo"
                 "stuartsierra-releases"  "http://stuartsierra.com/maven2"
                 "stuartsierra-snapshots" "http://stuartsierra.com/m2snapshots"}
  :source-path "src"
;  :aot  [gist.tool]
;  :main gist.tool
)