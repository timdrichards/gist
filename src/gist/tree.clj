(ns #^{:doc    "A library for working with GIST trees."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.tree)

(defn make-op
  "Makes a tree node with operation op and children c:
     (make-op 'add 3 4) => (add 3 4)"
  [o & c]
  (cons o c))

(defn get-children
  "Returns the children of tree node n."
  [n]
  (rest n))

(defn get-op
  "Returns the operation of tree node n."
  [n]
  (first n))

(defn op-node?
  "Returns true if the tree node is an operation."
  [n]
  (seq? n))

(defn leaf-node?
  "Returns true if the tree node is a leaf."
  [n]
  (not (op-node? n)))

(defn has-op?
  [n op]
  (and (op-node? n)
       (= (get-op n) op)))

(defn compiler-const?
  "Returns true if n is a compiler constant."
  [n]
  (and (op-node? n)
       (has-op? n 'cconst)))

(defn int-const?
  "Returns true if n is an integer constant."
  [n]
  (integer? n))

(defn float-const?
  "Returns true if n is a float constant."
  [n]
  (float? n))

(defn const?
  "Returns true if n is a constant."
  [n]
  (or (compiler-const? n)
      (int-const?      n)
      (float-const?    n)))

(defn param?
  "Returns true if n is a parameter:
     (param? '&imm) => true"
  [n]
  (if (symbol? n)
    (= (first (str n)) \&)
    false))

(defn store?
  "Returns true if n is a store:
     (store? '$C) => true"
  [n]
  (if (symbol? n)
    (= (first (str n)) \$)
    false))

(defn store-class?
  "Returns true if n is a store class:
     (store-class? '%R:rd) => true"
  [n]
  (if (symbol? n)
    (= (first (str n)) \%)
    false))

(defn memory?
  "Returns true if n is a store or store class."
  [n]
  (or (store? n)
      (store-class? n)))

(defn take-param
  "Returns the parameter part of a store class
   if n is a store class and n has a parameter
   specifier; nil otherwise:
      (take-param '%R:rd) => $rd"
  [n]
  (cond
   (not (store-class? n))        nil
   (= (.indexOf (str n) ":") -1) nil
   :else (let [s (str n)
               i (+ (.indexOf s ":") 1)
               p (.substring s i)]
           (symbol (str "$" p)))))

(defn op?
  "Returns true if n is an op node."
  [n]
  (and (not (const? n))
       (not (param? n))
       (not (store? n))
       (not (store-class? n))))

;; (def ops '[add addc sub mul div band bor bxor brsz brsz0 frsz frsz0
;;            conc eq ne gt lt le ge land lor lxor ror rol lsh rsh rsha
;;            sext zext lnot dec inc neg pos pop0 pop1 fadd fsub fmod fmul
;;            fdiv feq fne fgt flt fge fle fext fneg lget])
