(ns #^{:doc    "A one-way unifier for GIST trees."
       :author ["Elisabeth Baseman <ebaseman@cs.umass.edu>"]
       }
  gist.unifier
  (:use [gist.lang]))

(defn bound?
      [v, m]
      ((contains? m v) true false))

(defn bind
      [v, t, m]
      (assoc m v t))

(defn unify
  [x, y, m]
  (cond
   (and (op? x) (op? y)) (if (sameop x y) (unify (args x) (args y) m) false)
   (and (iconst? x) (iconst? y)) (if (= x y) m false)
   (and (fconst? x) (fconst? y)) (if (= x y) m false)
   (isvar?   x) (if (bound? x m) (unify (get m x) y) (bind x y m))
   :default false))




 
