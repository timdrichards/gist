(ns #^{:doc "Wist - Gist web interface core"
       :author "Prasanna Gautam <prasannagautam@gmail.com"}
  wist.core
  (:use compojure.core hiccup.core hiccup.page-helpers
        clojure.contrib.pprint )
  (:require gist.lang
            [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.contrib.string  :as st :only (butlast)]
            clojure.contrib.java-utils
            )
  (:import [java.io File FilenameFilter]))

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
  (cond 
    (symbol? code ) code
    (number? code) code
    (map? code)
    (with-out-str (doseq [key (keys code)]
                        (do
                          (if (or (map? (code key) ) (seq? (code key)))
                            (do
                              (println key)
                            (doseq [k (keys (code key))]
                              (println (format "%s-> %s" k (formatted-code ((code key) k ))))
                            ))
                          ( println (format "%s -> %s" key (formatted-code (code key))))
                          )
                        )))
    (seq? code) (with-out-str (pprint code))
  ))


(defn page-doesnt-exist []
  (view-layout [:h2 "Exists not the page looking for you are."]
               [:h3 "-- Master Yoda"]))

(defn display-machine [name]
  (binding [*ns* *ns*]
    (if (not 
          (. (clojure.contrib.java-utils/file (str "md/" name ".md")) exists))
        (view-layout 
          [:h2 "No such machine description exists"])
    ;else 
    (let [machine (gist.lang/load-machine (str "md/" name ".md"))]
      (view-layout [:h1 name ]
                   [:div.machine 
                    [:h2 "Parameters"]
                    [:p.params [:pre (formatted-code (:params machine))]]
                    [:h2 "Instructions"]
                    [:p.insts [:pre (formatted-code (:insts machine))]]
                    [:h2 "Types"]
                    [:p.types [:pre (formatted-code (:types machine))]]]
                   )))))

(defn show-machines-list []
  (let [dir (File. "md")
        fil (proxy [FilenameFilter] []
              (accept [dir name] (. name (endsWith ".md"))))      
        machines (map 
                   #(st/butlast 3 (. % getName)) 
                   (. dir (listFiles fil)))
        ]
    (view-layout [:ul
                  (for [machine machines]
                    [:li 
                     [:a.action {:href (str "/machine/" machine)} 
                      machine]
                     ])])
    ))


(defroutes main-routes
           (GET "/" []
                (show-machines-list))
           (GET "/machine/:name" [name] (display-machine name))
           (GET "/*"[] (page-doesnt-exist))
           )

(def app 
  (handler/site main-routes))
