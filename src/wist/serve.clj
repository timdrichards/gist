(ns wist.serve
  (:require wist.core)
  (:use ring.adapter.jetty))

(defn start-wist []
  (run-jetty #'wist.core/app {:port 8080})
  )

(defn -main [& args]
  (start-wist)
  )
