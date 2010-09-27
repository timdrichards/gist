(ns gist.tree
  (:use clojure.set clojure.contrib.pprint)
  (:require [gist.types :as types]
            [clojure.contrib.pprint :as pprint])
  (:import gist.types.ArrayType
           gist.types.BitType))  

;; semantic trees
(defrecord Instruction [name params seq])
(defrecord InstParams  [decs])
(defrecord Seq         [pars])
(defrecord Par         [asns])
(defrecord If          [cond stm])
(defrecord Asn         [lhs rhs])
(defrecord AliasRef    [name])
(defrecord StClassRef  [name])
(defrecord MemIdx      [name idx])
(defrecord MemRng      [name from to])
(defrecord BoolConst   [val])
(defrecord IntConst    [val])
(defrecord Param       [name])
(defrecord Op          [op rands])
(defrecord SemError    [msg])
(defrecord Store       [name kind size accs])
(defrecord Alias       [name store from to])
(defrecord StClass     [name exp])
(defrecord StClassAny  [class-list])

(defn make-error
  [msg]
  (SemError. msg))

(defn make-store
  ([name kind accs]
     (Store. name kind :infinite accs))
  ([name kind size accs]
     (Store. name kind size accs)))

(defn make-alias
  [name store from to]
  (Alias. name store from to))

(defn make-store-class
  [name exp]
  (StClass. name exp))

(defn make-store-class-any
  [class-list]
  (StClassAny. class-list))

(defn make-instruction
  [name params seq]
  (Instruction. name params seq))

(defn make-inst-params
  [decs]
  (InstParams. decs))

(defn make-seq
  [pars]
  (Seq. pars))

(defn make-par
  [asns]
  (Par. asns))

(defn make-if
  [cond stm]
  (If. cond stm))

(defn make-asn
  [lhs rhs]
  (Asn. lhs rhs))

(defn make-memidx
  [name idx]
  (MemIdx. name idx))

(defn make-memrng
  [name from to]
  (MemRng. name from to))

(defn make-bconst
  ([val type] (with-meta (BoolConst. val) {:type type}))
  ([val]      (make-bconst val types/no-type)))

(defn make-iconst
  ([val type] (with-meta (IntConst. val) {:type type}))
  ([val]      (make-iconst val types/no-type)))

(defn make-param
  ([val type] (with-meta (Param. val) {:type type}))
  ([val]      (make-param val types/no-type)))

(defn make-op
  ([op rands type] (with-meta (Op. op rands) {:type type}))
  ([op rands]      (make-op op rands types/no-type)))
