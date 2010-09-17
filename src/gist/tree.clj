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
(defrecord MRef        [name idx])
(defrecord BoolConst   [val])
(defrecord IntConst    [val])
(defrecord Param       [name])
(defrecord Op          [op rands])
(defrecord SemError    [msg])

;;;;;; STORE STUFF ;;;;;;
(defrecord Store       [name kind accs])
(defrecord Alias       [name store exp])
(defrecord StClass     [name exp]) 

;; store trees
;; (defrecord Store [name acc])
;; '(store M
;;    [word (array bit u l 32)
;;     half (array bit u l 16)
;;     byte (array bit u l 8 )])
;; '(store SP
;;    [word (array bit u l 32)])
;; '(class SSP SP  [0 5])
;; '(class EAX GPR [4])
;; '(class EAX/ESP GPR (- (.. 4 20) ))
;; '(store R 8
;;  [word (array bit u l 32)])

;; (defn reg-class-interp-vector
;;   [x]
;;   (set x))

;; (defn reg-class-interp-range
;;   [x]
;;   (let [s (nth x 1)
;;         e (inc (nth x 2))]
;;     (set (range s e))))

;; (defn reg-class-interp)

;; (defn reg-class-interp-diff
;;   [x]
;;   (let [o1 (reg-class-interp (nth x 1))
;;         o2 (reg-class-interp (nth x 2))]
;;     (difference o1 o2)))

;; (defn reg-class-interp-union
;;   [x]
;;   (let [o1 (reg-class-interp (nth x 1))
;;         o2 (reg-class-interp (nth x 2))]
;;     (union o1 o2)))
  
;; (defn reg-class-interp-op
;;   [x]
;;   (condp = (first x)
;;       '.. (reg-class-interp-range x)
;;       '-  (reg-class-interp-diff  x)
;;       '+  (reg-class-interp-union x)))

;; (defn reg-class-interp
;;   [x]
;;   (if (vector? x)
;;     (reg-class-interp-vector x)
;;     (reg-class-interp-op x)))

;;;;;; STORE STUFF ;;;;;;        

(defn make-error
  [msg]
  (SemError. msg))

(defn make-store
  [name kind accs]
  (Store. name kind accs))

(defn make-alias
  [name store exp]
  (Alias. name store exp))

(defn make-store-class
  [name exp]
  (StClass. name exp))

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

(defn make-mref
  [name idx]
  (MRef. name idx))

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
