(ns #^{:doc    "A library for type checking GIST instructions."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.typecheck
  (:use [gist.lang]
        [gist.type]
        [clojure.contrib.generic.math-functions :only (log ceil)]))

(defn log2
  [v]
  (ceil (/ (log v) (log 2))))

(defn typecheck
  [tree]
  (cond
   (binop? tree) (typecheck-binop tree)
   (unop?  tree) (typecheck-unop  tree)
   