(ns #^{:doc    "A library for matching GIST trees."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.match
  (:require gist.tree :as t))

(defn failed?
  [m]
  (:failed m))

(defn success?
  [m]
  (not (:failed m)))

(defn fail
  [m]
  (assoc m :failed true))

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
      (fail m))))

(defn match-op
  [s t m]
  (let [sop (t/get-op s)
        top (t/get-op t)]
    (if (= sop top)
      (match-children s t m)
      (fail m))))

(defn match-const
  [s t m]
  (if (= s t) m (fail m)))    

(defn bind-param
  [s t m]
  (let [bo (:bindings m)
        bn (assoc bo t s)]
    (assoc m :bindings bn)))

(defn match-tree
  [s t m]
  (cond
   (and (t/op-node? s) (t/op-node? t)) (match-op    s t m)
   (and (t/const?   s) (t/const?   t)) (match-const s t m)
   (and (any?       s) (t/param?   t)) (bind-param  s t m)
   :else (throw (Exception. (str "match-tree: unknown case: " s ", " t)))))

(defn match
  [s t]
  (match-tree s t {}))
              