(ns #^{:doc    "A library for working with GIST trees."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.canon
  (:require [gist.tree   :as tree]
            [gist.index  :as idx ]
            [clojure.set :as set ]))

(defn- dbg
  "A useful debugging function."
  [s n]
  n)

;;;; Dependency Analysis ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- depend-par-flat
  "Flattens multiple parallel blocks in a tree n
   into a tree with a single parallel block."
  [n]
  (tree/make-op 'seq
    (reduce #(tree/make-op* 'par
              (concat
                (tree/get-children %1)
                (tree/get-children %2)))
            (tree/get-children n))))

(defn- depend-par-vec
  "Returns a vector or guarded effects. It assumes the
   tree n has only one parallel block."
  [n]
  (let [par (tree/child n 1)
        cc  (tree/get-children par)]
    (vec cc)))

(defn- depend?
  "Returns true if n1 depends on n2."
  [n1 n2]
  (defn vars [n]
    (cond
     (tree/op-node? n) (map vars (tree/get-children n))
     (tree/param?   n) [n]
     (tree/memory?  n) [n]
     :else
     (throw "Unknown node kind")))

  (let [x (set (flatten (vars (tree/child n1 2))))
        y (set (flatten (vars (tree/child n2 1))))]
    (if (some #(y %) x) true false)))

(defn- depend-graph
  "Returns a dependency graph."
  [v]
  (defn scan [i j]
    (if (< j 0)
      []
      (let [n1 (nth v i)
            n2 (nth v j)]
        (cond
         (depend? n1 n2)  (cons j (scan i (dec j)))
         :else            (scan i (dec j))))))

  (loop [i (dec (count v))
         g v]
    (if (= i 0)
      (assoc g i [])
      (recur (dec i) (assoc g i (scan i (dec i)))))))
            

;;;; Ranking Analysis ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- rank+
  "Returns a new rank vector from node n and rank r.  A rank vector
   is a vector v where (first v) == n and (second v) == r."
  [n r]
  [n r])

(defn- node
  "returns the node of a rank vector."
  [v]
  (first v))

(defn- rank
  "Returns the rank of a rank vector."
  [v]
  (second v))

(defn- rank=
  "Returns true if the rank of v1 and v2 are the same"
  [v1 v2]
  (= (rank v1)
     (rank v2)))

(defn- maxr
  "Returns the maximum rank given a list of rank vectors."
  [vs]
  (apply max (map rank vs)))

(defn- cmpr
  "Compares the rank of rank vector v1 and v2. Returns -1 if v1's
   rank is less than v2's rank, 0 if they are the same, and 1 if
   v1's rank is larger than v2's rank."
  [v1 v2]
  (if (not (rank= v1 v2))
    (compare (rank v1) (rank v2))
    (let [n1 (node v1)
          n2 (node v2)]
      (cond
       (and (tree/int-const? n1)
            (tree/int-const? n2))
       (compare n1 n2)
       ; otherwise
       (and (tree/float-const? n1)
            (tree/float-const? n2))
       (compare n1 n2)
       ; otherwise
       (and (tree/compiler-const? n1)
            (tree/compiler-const? n2))
       (compare (str n1) (str n2))
       ; otherwise
       (tree/int-const? n1)
       -1
       ; otherwise
       (tree/int-const? n2)
       1
       ; otherwise
       (tree/float-const? n1)
       -1
       ; otherwise
       (tree/float-const? n2)
       1       
       ; otherwise
       (tree/compiler-const? n1)
       -1
       ; otherwise
       (tree/compiler-const? n2)
       1
       ; otherwise
       :else
       (compare (str n1) (str n2))))))

(defn- assoc?
  "Returns true if op is an associative operation."
  [op]
  (op #{'add  'mul 'band 'bor  'bxor
        'land 'lor 'fadd 'fmul}))

(defn- flatten-args
  "Returns a translated list of arguments xs that have been
  flattened based on the operator op.  This is a helper
  function used by the nary function.
    Example:
      (flatten-args 'add '((add 4 5) 6)) => '(add 4 5 6)"
  [op xs]
  (if (empty? xs)
    nil
    (let [x (first xs)
          r (rest  xs)]
      (if (and (tree/op? x)
               (= op (tree/get-op x)))
        (concat (tree/get-children x)
                (flatten-args op r))
        (cons x (flatten-args op r))))))

(defn- nary
  "Returns a tree with associative operations
  translated into their n-ary equivalent.  For example,
  if we have the tree (+ (+ x y) z), this function
  returns the tree (+ x y z)."    
  [n]
  (if (tree/op? n)
    (let [op (tree/get-op n)
          cc (tree/get-children n)
          fl (map nary cc)]
      (if (assoc? op)
        (tree/make-op* op
               (flatten-args op fl))
        (tree/make-op* op fl)))
    ; else
    n))

(defn- binary
  "returns the binary tree equivalent of the nary tree n."
  [n]
  (if (tree/op? n)
    (let [op (tree/get-op n)
          cc (tree/get-children n)]
      (if (and (assoc? op) (> (count cc) 2))
        (let [a1 (first cc)
              a2 (binary (tree/make-op* op (rest cc)))]
          (tree/make-op op a1 a2))
        (tree/make-op* op cc)))
    ; else
    n))
          
(defn- rank-tree
  "Returns a tree whose operands have been sorted
  by the following ranking scheme:
    - Constants receive rank 0.
    - Instruction parameters receive rank 1.
    - Memory references receive rank 2.
    - An expression tree receives a rank equal to its
      highest ranked operand."
  [n]
  (cond
   (tree/const?        n) (rank+ n 0)
   (tree/param?        n) (rank+ n 1)
   (tree/store?        n) (rank+ n 2)
   (tree/store-class?  n) (rank+ n 3)
   :else (let [op (tree/get-op n)
               rv (map rank-tree (tree/get-children n)) ; rank children
               sv (if (assoc? op) (sort cmpr rv) rv)    ; sort ranked children
               mr (maxr sv)                             ; max rank
               cc (map node sv)]                        ; extract nodes
           (rank+ (tree/make-op* op cc) mr))))     ; rank node

(defn rank-expr
  "Returns a ranked expression."
  [n]
  (binary (node (rank-tree (nary n)))))

(defn- canon-geff
  "Returns a guarded effect in canonical form."
  [n]
  (let [guard  (rank-expr (tree/child n 1))
        effect (rank-expr (tree/child n 2))]
    (tree/make-op '<- guard effect)))

(defn- canon-par
  "Returns a parallel block in canonical form."
  [n]
  (tree/make-op*
   'par
   (map canon-geff
        (tree/get-children n))))

(defn- canon-seq
  "Returns a sequence in canonical form."
  [n]
  (tree/make-op*
   'seq
   (map canon-par
        (tree/get-children n))))

(defn canon
  "Returns the tree t in canonical form."
  [t]
  (canon-seq t))

; sample trees
(def a '(add (add 4 5) (sub 6 7)))
(def b '(add (add &x 3) (sub &x 4)))
(def c '(add (add $x 3) (sub $x 4)))
(def d '(add (add %R 3) (sub %R 4)))
(def e '(add (add %R &x) (sub %R &x)))
(def f '(add (add %R $x) (sub $x %R)))

;; (def i (gist.lang/get-machine-inst
;;         (gist.lang/load-machine "md/arm.md")
;;         'adcis))