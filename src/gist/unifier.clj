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

(def unify-args)

(defn unify
  [x, y, m]
  (cond  
   (and (is-iconst? x) (is-iconst? y)) (if (= x y) m false)
   (and (is-fconst? x) (is-fconst? y)) (if (= x y) m false)
   (is-varn?   x) (if (isbound? m x) (unify (get m x) y) (bind x y m))
   (and (op? x) (op? y)) (if (sameop x y) (unify-args (args x) (args y) m) false)
   :default false))

;;need to go through and unify each args




 
