(ns #^{:doc    "A library for type checking GIST instructions."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.type
  (:require [gist.tree :as t])
  (:use [gist.lang]))

;;;; type definitions ;;;;

(defn make-type
  [k i]
  {:type k :info i})

(defn type-kind
  [t]
  (:type t))

(defn error-type
  [m]
  (make-type :error m))

(defn seq-type
  []
  (make-type :seq :seq))

(defn par-type
  []
  (make-type :par :par))

(defn geff-type
  []
  (make-type :-> :->))

(defn asn-type
  []
  (make-type :<- :<-))

(defn bit-type
  []
  (make-type :bit :bit))

(defn number-type
  []
  (make-type :number :number))

(defn float-type
  [w]
  (make-type
   :float
   {:kind  754
    :width w}))

(defn array-type
  [e s w]
  (make-type
   :array
   {:endian e
    :signed s
    :width  w}))

;;;; type predicates ;;;;

(defn istype?
  [t k]
  (= (:type t) k))

(defn number-type?
  [t]
  (istype? t :number))

(defn bit-type?
  [t]
  (istype? t :bit))

(defn array-type?
  [t]
  (istype? t :array))

(defn float-type?
  [t]
  (istype? t :float))

(defn same-type?
  [t1 t2]
  (= (type-kind t1)
     (type-kind t2)))

(defn set-type
  [n t]
  (let [m (meta n)]
    (with-meta n
      (assoc m :type t))))

(defn get-type
  [n]
  (:type (meta n)))

;;;;  type checking rules ;;;;

(defn type-error
  [n & m]
  (let [v (apply str (concat ["type error: "] m))]
    (println v)
    (set-type n (error-type v))))

(defn type-store
  [n e]
  (let [ty (n e)]
    (if ty
      (set-type n ty)
      (type-error n "unknown type for store: " n))))

(defn type-store-class
  [n e]
  (let [ty (n e)]
    (if ty
      (set-type n ty)
      (type-error n "unknown type for store class: " n))))

(defn type-int
  [n e]
  n)

(defn type-float
  [n e]
  n)

(defn type-param
  [n e]
  (let [ty (n e)]
    (if ty
      (set-type n ty)
      (type-error n "unknown type for parameter: " n))))

(defn type-exp)

(defn type-op
  [n e]
  (let [op     (nth n 0)
        args   (rest n)
        texp   (map #(type-exp % e) args)
        types  (map get-type texp)
        passed (empty? (for [x types y types :when (not= x y)] [x y]))]
    (if passed
      (cons op texp)
      (type-error n "type error on " n
                  " with types " passed))))

;;;; Stopped Here!  Still need to update everything below this line ;;;;

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
    (set-type
     (par (map type-geff args))
     (par-type))))

(defn type-seq
  [n]
  (let [args (rest n)]
    (set-type
     (seq (map type-par args))
     (seq-type))))
     
(defn type-inst
  [i]
  (assoc i
    :semantics
    (type-seq
     (get i :semantics))))