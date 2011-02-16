(defproject gist "0.0.1"
  :description "An automatic code generator generator"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [matchure "0.10.1"]]
  :dev-dependencies [[swank-clojure "1.2.0"]]
  :repositories {"clojure-releases"
                 "http://build.clojure.org/releases"}
  :source-path "src"
;  :aot  [gist.tool]
;  :main gist.tool
)