(ns #^{:doc "Wist - Gist web interface core"
       :author "Prasanna Gautam <prasannagautam@gmail.com"}
  wist.core
  (:use compojure.core hiccup.core hiccup.page-helpers
        clojure.contrib.string  clojure.pprint)
  (:import [java.io File FilenameFilter]))
(require 'gist.lang)
(defn view-layout [& content]
  (html
    (doctype :xhtml-strict)
    (xhtml-tag "en"
               [:head
                [:meta {:http-equiv "Content-type"
                        :content "text/html;charset=utf-8"}]
                [:title "WIST"]
                [:body content]])))

(defn display-machine [name]
  (let [ machine (gist.lang/load-machine (str "md/" name ".md"))]
  (view-layout [:h1 "test" ]
    [:p.machine machine]
  )))

(defn show-machines-list []
  (let [dir (File. "md")
        fil (proxy [FilenameFilter] []
              (accept [dir name] (. name (endsWith ".md"))))      
        machines (map 
                   #(butlast 3 (. % getName)) 
                   (. dir (listFiles fil)))
        ]
    (view-layout [:ul
        (for [machine machines]
          [:li 
          [:a.action {:href (str "/machine/" machine)} 
            machine]
          ])])
   ))


(defroutes app
           (GET "/" []
                (show-machines-list))
           (GET "/machine/:name" [name] (display-machine name))
           )

