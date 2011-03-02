(ns #^{:doc    "A library for type checking GIST instructions."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.type
  (:require [gist.tree :as t])
  (:use [gist.lang]
        [clojure.contrib.str-utils :only (str-join)]))

;;;; type definitions ;;;;

(defn make-type
  "Returns a type of kind k with info i."
  [k i]
  {:type k :info i})

(defn type-kind
  "Returns the kind of type t."
  [t]
  (:type t))

(defn type-info
  "Returns the info of type t."
  [t]
  (:info t))

(defn unknown-type
  "Returns an unknown type."
  [m]
  (make-type :unknown m))

(defn error-type
  "Returns an error type."
  [m]
  (make-type :error m))

(defn seq-type
  "Returns a sequence type."
  []
  (make-type :seq :seq))

(defn par-type
  "Returns a parallel type."
  []
  (make-type :par :par))

(defn geff-type
  "Returns a guarded effect type."
  []
  (make-type :-> :->))

(defn asn-type
  "Returns an assignment type."
  []
  (make-type :<- :<-))

(defn bit-type
  "Returns a bit type."
  []
  (make-type :bit :bit))

(defn int-type
  "Returns an integer type."
  []
  (make-type :int :int))

(defn float-type
  "Returns a float type with width w."
  [w]
  (make-type
   :float
   {:kind  754
    :width w}))

(defn array-type
  "Returns an array type with endianess e, signedness s,
   and width w."
  [e s w]
  (make-type
   :array
   {:endian e
    :signed s
    :width  w}))

;;;; type predicates ;;;;

(defn istype?
  "Returns true if type is of kind k."
  [t k]
  (= (:type t) k))

(defn error-type?
  "Returns true if t is an error type."
  [t]
  (istype? t :error))

(defn int-type?
  "Returns true if t is an integer type."
  [t]
  (istype? t :int))

(defn bit-type?
  "Returns true if t is a bit type."
  [t]
  (istype? t :bit))

(defn array-type?
  "Returns true if t is an array type."
  [t]
  (istype? t :array))

(defn float-type?
  "Returns true if t is a float type."
  [t]
  (istype? t :float))

(defn same-type?
  "Returns true if t1 and t2 are of the same type."
  [t1 t2]
  (= (type-kind t1)
     (type-kind t2)))

(defn set-type
  "Returns a new tree node constructed from n
   with its type set to t."
  [n t]
  (let [m (meta n)]
    (with-meta n
      (assoc m :type t))))

(defn get-type
  "Returns the type of tree node n."
  [n]
  (cond
   (t/int-const?   n) (int-type)
   (t/float-const? n) (float-type)
   :else              (:type (meta n))))

(defn repr
  "Returns the string representation of type t."
  [t]
  (let [i (type-info t)]
    (cond
     (error-type? t)  (str "error-type")
     (int-type?   t)  "int-type"
     (float-type? t)  "float-type"
     (array-type? t)  (str "array-type<" (:endian i) "," (:signed i) "," (:width i) ">")
     :else
     (str t))))

;;;;  type checking rules ;;;;

(defn err
  "Prints message m and returns m as a string."
  [& m]
  (let [v (apply str (cons "type error: " m))]
    (println v)
    v))

(defn type-error
  "Prints a type error and returns the error type."
  [n & m]
  (let [v (apply err m)]
    (set-type n (error-type v))))

(defn unify-types
  "Returns the unified type of type t1 and type t2."
  [t1 t2]

  (defn fail []
    (let [v (err "type unification failed: "
                 t1 " != " t2)]
      (error-type v)))

  (defn pick []
    (let [w1 (:width (type-info t1))
          w2 (:width (type-info t2))]
      (cond
       (= w1 w2) t1
       (< w1 w2) t2
       (> w1 w2) t1)))
      
  (cond
   (and (array-type? t1) (array-type? t2)) (pick)
   (same-type? t1 t2) t1
   (and (int-type?   t1) (float-type? t2)) (fail)
   (and (float-type? t1) (int-type?   t2)) (fail)
   (int-type?   t1)   t2
   (int-type?   t2)   t1
   (float-type? t1)   t2
   (float-type? t2)   t1
   :else
   (let [v (err "type unification failed: "
                "unknown case: "
                t1 " =? " t2)]
     (error-type v))))

(defn type-store
  "Returns a typed store reference."
  [n e]
  (let [ty (n e)]
    (if ty
      (set-type n ty)
      (type-error n "unknown type for store: " n))))

(defn type-store-class
  "Returns a typed store class reference."
  [n e]
  (let [ty (n e)]
    (if ty
      (set-type n ty)
      (type-error n "unknown type for store class: " n))))

(defn type-int
  "Returns a typed integer."
  [n e]
  n)

(defn type-float
  "Returns a typed float."
  [n e]
  n)

(defn type-cconst
  "Returns a typed compiler constant."
  [n e]
  (set-type n (int-type)))

(defn type-param
  "Returns a typed parameter."
  [n e]
  (let [ty (n e)]
    (if ty
      (set-type n ty)
      (type-error n "unknown type for parameter: " n))))

;; forward definition.
(defn type-exp)

(defn type-op
  "Returns a typed operation."
  [n e]
  (let [op    (t/get-op n)
        args  (t/get-children n)
        texp  (map #(type-exp % e) args)
        types (map get-type texp)
        type  (reduce unify-types (map get-type texp))]
    (cond
     (empty?      args)       (type-error n "unknown type for " n)
     (some error-type? types) (set-type (t/make-op op texp) (error-type "type-op"))
     (error-type? type)       (error-type n "type error on " n " with type " type)
     :else                    (set-type (t/make-op op texp) type))))

(defn type-exp
  "Returns a typed expression."
  [n e]
  (cond
   (t/int-const?      n) (type-int         n e)
   (t/float-const?    n) (type-float       n e)
   (t/compiler-const? n) (type-cconst      n e)
   (t/param?          n) (type-param       n e)
   (t/store?          n) (type-store       n e)
   (t/store-class?    n) (type-store-class n e)
   (t/op-node?        n) (type-op          n e)
   :else
   (type-error n "unknown node kind: " n)))

(defn type-asn
  "Returns a typed assignment."
  [n e]
  (let [c  (t/get-children n)
        l  (type-exp (first  c) e)
        r  (type-exp (second c) e)
        ty (unify-types l r)]
    (cond
     (error-type? ty) (set-type (t/make-op '<- l r))
     :else            (set-type (t/make-op '<- l r) l))))
  
(defn type-geff
  "Returns a typed guarded effect."
  [n e]
  (let [c (t/get-children n)
        b (type-exp (first  c) e)
        f (type-asn (second c) e)
        r (t/make-op '-> b f)]
    (cond
     (error-type? b) (set-type r (error-type "guarded effect"))
     (error-type? f) (set-type r (error-type "guarded effect"))
     (same-type? b (bit-type)) (set-type r (asn-type))
     :else
     (type-error n "type for guard test is not bit: " n))))

(defn type-par
  "Returns a typed parallel block."
  [n]
  (let [args (rest n)]
    (set-type
     (par (map type-geff args))
     (par-type))))

(defn type-seq
  "Returns a typed sequence."
  [n]
  (let [args (rest n)]
    (set-type
     (seq (map type-par args))
     (seq-type))))
     
(defn type-inst
  "Returns a typed instruction."
  [i]
  (assoc i
    :semantics
    (type-seq
     (get i :semantics))))