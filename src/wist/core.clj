(ns #^{:doc "Wist - Gist web interface core"
       :author "Prasanna Gautam <prasannagautam@gmail.com"}
  wist.core
  (:use compojure.core hiccup.core hiccup.page-helpers
        clojure.contrib.string  clojure.contrib.pprint)
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

(defn formatted-code [code]
  [:pre (with-out-str (pprint code))]
  )

(defn display-machine [name]
  (binding [*ns* *ns*]
  (let [machine (gist.lang/load-machine (str "md/" name ".md"))]
    (println (pprint machine))
    (view-layout [:h1 "test" ]
    [:div.machine [:p.name name]
     [:h2 "Parameters"]
     [:p.params (formatted-code (:params machine))]
     [:h2 "Instructions"]
     [:p.insts (formatted-code (:insts machine))]
     [:h2 "Types"]
      [:p.types (formatted-code (:types machine))]]
  ))))

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

