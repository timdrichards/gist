(ns #^{:doc    "A one-way unifier for GIST trees."
       :author ["Elisabeth Baseman <ebaseman@cs.umass.edu>"]
       }
  gist.unifier
  (:use [gist.lang]))

(defn isbound?
      [m, v]
      (contains? m v))

(defn bind
      [v, t, m]
      (assoc m v t))

(defn is-empty?
      [x]
      (== (count x) 0))

(def unify-args)

(defn unify
  [x y m]
 ;; (println x "," y "," m)
  (cond  
   ;;(and (is-empty? x) (is-empty? y)) m
   (and (is-iconst? x) (is-iconst? y)) (if (= x y) m false)
   (and (is-fconst? x) (is-fconst? y)) (if (= x y) m false)
   (is-varn?   x) (if (isbound? m x) (unify (get m x) y) (bind x y m))
   (and (op? x) (op? y)) (if (sameop x y) (unify-args (args x) (args y) m) false)
   :default false))

(defn unify-args
      [xargs yargs m]
     ;; (println "xargs:" xargs)
      (cond
	(not (== (count xargs) (count yargs))) false
	(== (count xargs) 0) m
	:default (let [b (unify (first xargs) (first yargs) m)] 
          (if (not b)
	      false
	      (unify-args (rest xargs) (rest yargs) b)))))
      




 
