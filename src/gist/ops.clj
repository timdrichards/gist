(ns #^{:doc    "A library for defining GIST operations."
       :author ["Tim Richards <richards@cs.umass.edu>",
                "Elisabeth Baseman <ebaseman@cs.umass.edu>"]
       }
  gist.ops)

;;;; Op Data Structure Abstraction ;;;;

(defn make-op
  [op args]
  (cons op args))

(defn args
  [t]
  (rest t))

(defn argn
  [t index]
  (nth (args t) index)) 

(defn getop
  [t]
  (first t))

(defn set-type
  "Returns a new tree node constructed from n
   with its type set to t."
  [n t]
  (let [m (meta n)]
    (with-meta n
      (assoc m :type t))))

(defn get-type
  "Returns the type of tree t."
  [t]
  (:type (meta t)));

;;;; Op Language Predicates ;;;;

(defn op?
  [t]
  (and (seq? t)
       (symbol? (first t))))

(defn binop?
  [t]
  (and (op? t)
       (= (count (args t)) 2)))

(defn unop?
  "Returns true if tree t is a unary op."
  [t]
  (and (op? t)
       (= (count (args t)) 1)))
       
(defn sameop?
  "Returns true if tree t1 and t2 have the same op."
  [t1 t2]
  (= (getop t1)
     (getop t2)))

(defn hasop?
  "Returns true if the tree t has the given op."
  [t op]
  (= (getop t) op))

(defn variable?
  "Returns true if the tree t is a variable."
  [t]
  (if (symbol? t)
    (= \$ (first (seq (str t))))
    false))

(defn isconst?
  "Returns true if the tree t is a constant."
  [t]
  (and (op? t)
       (or (hasop? t 'iconst)
           (hasop? t 'fconst))))

;;;; Op Description Language Definitions ;;;;

(defn var-name
  [v]
  (let [s (seq (str v))  ; to sequence
        n (rest s)]      ; var name
    (symbol (apply str n))))

(defn check-tree
  [t]
  (cond
    (op?       t) (map check-tree t)
    (integer?  t) (make-op 'iconst (list t))
    (variable? t) (make-op 'varn (list (var-name t)))
    :default   t))

(defn check-args
  [op args]
  (if (not (or (= op 'iconst)
               (= op 'varn)))
    (map check-tree args)
    args))

