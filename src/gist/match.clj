(ns #^{:doc    "A library for matching GIST trees."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.match
  (:require [gist.tree :as t]
            [gist.type :as ty])
  (:use [gist.constraint]))

(defn add-constraint
  [c m]
  (let [cc (:constraints m)]
    (assoc m :constraints
           (cons c cc))))

(defn failed?
  [m]
  (some #(= (:kind %) :false) (:constraints m)))

(defn success?
  [m]
  (not (failed? m)))

(defn fail
  [m r]
  (add-constraint (!> :false r) m))

(defn any?
  [x]
  true)

;; forward reference
(defn match-tree)

(defn match-seq
  [x y m]
  (if (empty? x)
    m
    (let [nx (first x)
          ny (first y)
          mp (match-tree nx ny m)]
      (if (success? mp)
        (match-seq (rest x) (rest y) mp)
        mp))))

(defn match-children
  [s t m]
  (let [sn (t/get-children s)
        tn (t/get-children t)
        sc (count sn)
        tc (count tn)]
    (if (== sc tc)
      (match-seq sn tn m)
      (fail m (str "children mismatch: " sn " != " tn)))))

;; (defn match-types
;;   [st tt m]
;;   (cond
;;    (and (ty/number? st
(defn match-types
  [st tt m]
  ;; need to figure out how to do this...
  m)

(defn match-ops
  [s t m]
  (if (= (t/get-op s) (t/get-op t))
    (let [ts (ty/get-type s)
          tt (ty/get-type t)]
      (match-types ts tt m))
    (fail m (str "operation mismatch: " s " != " t))))

(defn match-node
  [s t m]
  (let [r (match-ops s t m)]
    (if (success? r)
      (match-children s t m)
      r)))

(defn match-const
  [s t m]
  (if (= s t) m (fail m (str "constant mismatch: " s " != " t))))

(defn bind-param
  [s t m]
  (add-constraint (!> :binding [s t]) m))

(defn match-tree
  [s t m]
  (cond
   (and (t/op-node? s) (t/op-node? t)) (match-node  s t m)
   (and (t/const?   s) (t/const?   t)) (match-const s t m)
   (and (t/const?   s) (t/param?   t)) (bind-param  s t m)
   (and (t/param?   s) (t/const?   t)) (add-constraint (!> :param [s t]) m)
   :else (throw (Exception. (str "match-tree: unknown case: " s ", " t)))))

(defn match
  [s t]
  (let [r (match-tree s t {})]
    (if (and (empty? r) (success? r))
      (add-constraint (!> :true :true) r)
      r)))
              