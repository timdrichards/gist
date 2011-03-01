(ns #^{:doc "A library for indexing GIST trees."
       :author "Prasanna Gautam <prasannagautam@gmail.com>"}
  gist.index
  (:require [gist.tree :as tree]))

;; A collection of large-ish primes that we use to compute
;; the hash value for each child in a semantic tree.
(def primes
     [3853 3833 3863 3877 3881
      3889 3851 3907 3911 3917
      3919 3923 3929 3931 3943
      3947 3967 3989 4001 4003])    

(defn ignore?
  "Returns true if n can be ignored."
  [n]
  (or (tree/const? n)
      (tree/param? n)
      (tree/store? n)
      (tree/store-class? n)))

(defn skip?
  "Returns true if n can be skipped."
  [n]
  (and (tree/op? n)
       (or (tree/has-op? n 'sext)
           (tree/has-op? n 'fext))))

;; ** Shape Notes **
;; The shape of a tree is based on the operators that appear in the
;; tree.  That is, (add (sub x y) 5) will have a different hash value
;; than (sub (add x y) 5) because the 'sub' and 'add' operators appear
;; at different levels in the tree.  We ignore tree nodes such as
;; constants, parameters, stores, store classes, and sign extension
;; because they can match target trees in a variety of ways.  For
;; example, a store class representing a set of registers can match
;; a specific register that exists in that class.
(defn shape
  "Returns a integer value representing the shape of the tree n.
   The shape of a tree is a hash value based on operators that
   appear in the tree."
  [n]
  (cond
   (ignore?  n) 0
   (skip?    n) (reduce + 0 (map shape (tree/get-children n)))
   (tree/op? n) (let [ho (.hashCode (tree/get-op n))       ; hashcode of op
                      hc (map shape (tree/get-children n)) ; hashcodes of children
                      hn (map * hc primes)]                ; multiply through primes
                  (reduce + ho hn))                        ; reduce with +
   :else
   (throw (Exception. (str "Unknown tree node: " n)))))

