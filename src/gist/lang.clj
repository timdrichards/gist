(ns gist.lang)

(defmacro type
  [e s w]
  `{:endian '~e
    :signed '~s
    :width  ~w})

(defn param
  [n t]
  {:name n
   :type t})

(defn inst
  [n p s]
  {:name n
   :params p
   :semantics s})

(def types  (atom {}))
(def params (atom {}))
(def insts  (atom {}))

(defn add-type
  [name type]
  (reset! types (conj @types {name type})))

(defn lookup-type
  [name]
  (@types name))

(defn add-param
  [name type]
  (reset! params (conj @params {name (param name type)})))

(defn lookup-param
  [name]
  (@params name))

(defn add-inst
  [name params semantics]
  (reset! insts (conj @insts {name (inst name params semantics)})))

(defn lookup-inst
  [name]
  (@insts name))

(defmacro deftype
  [n t]
  `(add-type '~n ~t))

(defmacro defparam
  [n t]
  (cond
   (symbol? t) `(add-param '~n (lookup-type '~t))
   :else       `(add-param '~n ~t)))

(defmacro defop
  [n]
  `(defmacro ~n
     [& args#]
     (let [a# (map (fn [x#] (if (symbol? x#)
                            (list 'quote x#)
                            x#)) args#)
           b# '~n]
       `(list '~b# ~@a#))))

;; (defmacro add
;;   [& args]
;;   (let [a (map (fn [x] (if (symbol? x)
;;                   `(quote ~x)
;;                   x)) args)]
;;     `(list (symbol "add") ~@a)))

;; '(add %r (neg 7))

(defmacro definst
  [n p s]
  `(add-inst '~n '~p '~s))
  
;; description below

(deftype lu4 (type little unsigned 4))
(deftype lu1 (type little unsigned 1))
(deftype lu8 (type little unsigned 8))

(defparam rd   lu4)
(defparam rn   lu4)
(defparam rs   lu4)
(defparam imm8 lu8)
(defparam s    lu1)

(definst adci
  [rn rd imm8]
  (seq
   (par (<- %R:rd (addc %R:rn %I:imm8 C))
        (<- C (addc %R:rn %I:imm8 C)))))

(definst adcis
  [rn rd imm8 s]
  (seq
   (par (<- %R:rd (addc %R:rn %I:imm8 C))
        (<- N (addc %R:rn %I:imm8 C))
        (<- Z (addc %R:rn %I:imm8 C))
        (<- C (addc %R:rn %I:imm8 C))
        (<- V (addc %R:rn %I:imm8 C)))))