(ns gist.cisl-01)

(defstruct node
  :kind
  :type
  :children)

(defstruct ctype
  :base
  :size
  :signedness
  :endianness)

(def bit    (struct ctype nil 1 nil nil))
(def uli32  (struct ctype bit 32 :unsigned :little))
(def ubi32  (struct ctype bit 32 :unsigned :big))
(def sli32  (struct ctype bit 32 :signed   :little))
(def sbi32  (struct ctype bit 32 :signed   :big))

(defmacro add
  ([a b] `(struct node :add 

; ast node definition
(defstruct node
  :kind
  :type
  :children)


; Macro definition for creating an AST node.
(defmacro defnode
  [name & keys]
  `(let [sname# (create-struct :kind ~@(map (fn [n] (keyword (str n))) keys) :type)]
     ~@(map (fn [n] `(defn ~(symbol (str "get-" n)) [s#]
			  (~(keyword (str n)) s#))) keys)
     (defn ~name
       ([~@keys]   (struct sname# ~(keyword (str name)) ~@keys :none))
       ([~@keys t#] (struct sname# ~(keyword (str name)) ~@keys t#)))
     (defn ~(symbol (str name "?"))
       [n#]
       (= (:kind n#) ~(keyword (str name))))
     '~name))

(defnode add exp1 exp2)
(def n1 (add 4 5))
(def n2 (add 6 7))
(add? n1)
(get-exp1 n1)
(get-exp2 n1)

