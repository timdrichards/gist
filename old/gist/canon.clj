(ns gist.canon
  (:require [gist.tree :as tree]))

(defn assoc?
  "Returns true if the tree represents an associative
  operation."
  [t]
  (if (seq? t)
    (let [op (first t)]
      (or (= op '+)
          (= op '*)))
    false))

(defn- flatten-args
  "Returns a translated list of arguments that have been
  flattened based on the operator op.  This is a helper
  function used by nary-tree."
  [op args]
  (if (empty? args)
    args
    (let [arg (first args)]
      (if (and (tree/op? arg)
               (= (tree/op arg) op))
        (concat (tree/children arg)
                (flatten-args op (rest args)))
        (cons arg
               (flatten-args op (rest args)))))))

(defn nary-tree
  "Returns a tree with associative operations
  translated into their n-ary equivalent.  For example,
  if we have the tree (+ (+ x y) z), this function
  returns the tree (+ x y z)."  
  [t]
  (if (tree/atom? t)
    t
    (let [nary (map nary-tree (tree/children t))
          op   (tree/op t)]
      (if (assoc? t)
        (list* op (flatten-args op nary))
        (list* op nary)))))

(defn bin-tree
  "Returns a tree that has been translated from
   nary-form to binary form."
  [t]
  (cond
   (and (assoc? t)
        (> (count (tree/children t)) 2))
   (let [op (tree/op t)
         ch (tree/children t)
         a1 (first ch)
         a2 (bin-tree (list* op (rest ch)))]
     (list op a1 a2))

   (tree/op? t)
   (let [op (tree/op t)
         ch (bin-tree (tree/children t))]
     (list* op ch))

   :else t))

(defn rank-tree
  "Returns a tree whose operands have been sorted
  by the following ranking scheme:
    - Constants receive rank 0.
    - Instruction parameters receive rank 1.
    - Memory references receive rank 2.
    - An expression tree receives a rank equal to its
      highest ranked operand."
  [t]
  (defn rank [t]
    (cond
     (tree/const? t) (list t 0)
     (tree/param? t) (list t 1)
     (tree/mem?   t) (list t 2)
     :else (let [a (map rank (tree/children t))
                 b (sort #(< (last %1) (last %2)) a)
                 m (last (last b))
                 c (map #(first %) b)]
             (list (list* (tree/op t) c) m))))
  (first (rank t)))

(defn canon
  "Returns the tree t in canonical form."
  [t]
  (bin-tree (rank-tree (nary-tree t))))

;; test trees
(def x '(+ 4 (+ (- %m 1) (+ $a 8))))
(def y '(+ 4 (+ (+ $a 8) (- %m 1))))
