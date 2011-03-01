(ns #^{:doc    "A library for talking about constraints."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.constraint
  (:require [gist.tree :as t]))

(defn constraint
  [k i]
  {:kind k
   :info i})

(def !> constraint)
