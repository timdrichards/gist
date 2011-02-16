(ns #^{:doc    "A library for type checking GIST instructions."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.type
  (:use [gist.lang]))

(defn type-symbol
  [n]
  n)

(defn type-number
  [n]
  (with-meta n {:type :number}))

(defn type-op
  [n]
  (let [op (nth n 0)
        args (rest n)]
    (cons op (map type-exp args))))

(defn type-exp
  [n]
  (cond
   (symbol? n) (type-symbol n)
   (number? n) (type-number n)
   (list?   n) (type-op n)
   :else n))

(defn type-asn
  [n]
  (let [l (nth n 1)
        r (nth n 2)]
    (with-meta
      (list '<-
        (type-exp l)
        (type-exp r))
      {:type :effect})))

(defn type-geff
  [n]
  (let [cond (nth n 1)
        effect (nth n 2)]
    (with-meta
      (list '->
         (type-exp cond)
         (type-asn effect))
      {:type :geff})))

(defn type-par
  [n]
  (let [args (rest n)]
    (with-meta
      (cons 'par (map type-geff args))
      {:type :par})))

(defn type-seq
  [n]
  (let [args (rest n)]
    (with-meta
      (cons 'seq (map type-par args))
      {:type :seq})))
    