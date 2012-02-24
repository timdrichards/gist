(ns #^{:doc    "A library for defining GIST instructions."
       :author ["Tim Richards <richards@cs.umass.edu>",
                "Elisabeth Baseman <ebaseman@cs.umass.edu>"]
       }
  gist.lang
  (:require (gist [ops     :as ops]
                  [type    :as type]
                  [store   :as store])))

;;;; Machine Language & Environment ;;;;
(def *env-machines* (ref {}))
(def *env-current*  (ref nil))

(defn env-add-type
  [name type]
  (let [machine (@*env-machines* @*env-current*)
        types   (machine :types)]
    (dosync
     (ref-set types
              (assoc @types
                name type)))))

(defn env-add-store
  [name store]
  (let [machine  (@*env-machines* @*env-current*)
        memories (machine :memories)]
    (dosync
     (ref-set memories
              (assoc @memories
                name store)))))

(defn env-add-inst
  [name inst]
  (let [machine  (@*env-machines* @*env-current*)
        insts (machine :insts)]
    (dosync
     (ref-set insts
              (assoc @insts
                name inst)))))


(defn load-machine
  [file]
  (binding [*ns* (the-ns 'gist.lang)]
    (do (load-file file)
        'done.)))

(defmacro machine
  [name]
  (in-ns '~name))

(defmacro machineo
  [name]
  `(do
     (create-ns '~name)
     (binding [*ns* (the-ns '~name)]
       (dosync
        (ref-set *env-machines*
                 (assoc @*env-machines*
                   '~name {:types    (ref {})
                           :memories (ref {})
                           :insts    (ref {})})))
       (dosync
        (ref-set *env-current* '~name))
       '~name)))

(defn machine-get
  "Return the machine given the symbol name."
  [name]
  (let [m @*env-machines*]
    (m name)))

(defn machine-get-types
  [name]
  (deref (:types (machine-get name))))

(defn machine-get-memories
  [name]
  (deref (:memories (machine-get name))))

(defn machine-get-insts
  [name]
  (deref (:insts (machine-get name))))
                     
;;;; Operation Language ;;;;

(defmacro defop
  "defop is used to define new gist operations.  The form
   (defop <name>) will create a macro (<name> args) used for 
   constructing instances of that operation with the given
   args to the operation.  It will also create a predicate 
   function (is-<name>? t) that will return true if t is a
   <name> operation."
  [op]
  (let [pname (symbol (str "is-" op "?"))] 
    `(do 
       ; This defines the constructor macro.
       (defmacro ~op
         [& args#]
         (let [cargs#  (ops/check-args '~op args#)]
            `(ops/make-op '~'~op '~cargs#)))

       ; This defines the predication function.
       (defn ~pname
         [t#]
         (= (ops/getop t#) '~op)) 
     )))       

;; Op Definitions

; Statements:
(defop goto)
(defop nop)
(defop par)
(defop gact)
(defop lset)
(defop excp)

; Locations:
(defop mem)
(defop iconst)
(defop fconst)
(defop param)

; Arithmetic:
(defop add)
(defop sub)
(defop modu)
(defop mul)
(defop div)

(defop fadd)
(defop fsub)
(defop fmod)
(defop fmul)
(defop fdiv)
(defop fext)

; Bitwise:
(defop rotl)
(defop rotr)
(defop shl)
(defop shr)
(defop shra)
(defop band)
(defop bor)
(defop bxor)

; Logical:
(defop eq)
(defop neq)
(defop ge)
(defop le)
(defop gt)
(defop lt)
(defop land)
(defop lor)
(defop lxor)

; Floating Logical:
(defop feq)
(defop fneq)
(defop fge)
(defop gle)
(defop fgt)
(defop flt)
(defop fland)
(defop flor)
(defop flxor)

; Extra:
(defop conc)
(defop sext)
(defop frsz)
(defop frsz0)
(defop brsz)
(defop brsz0)
(defop zext)
(defop varn)

; Unary:
(defop bnot)
(defop decr)
(defop incr)
(defop lnot)
(defop neg)
(defop pos)
(defop pop0)
(defop pop1)
(defop fneg)

; Memory:
(defop lget)
(defop lgetr)

; Compiler Constant:
(defop ccnst)


;;;; Type Language ;;;;
(defmacro defty
  "Defines a new type named name."
  [name ty]
  `(do
     (def ~name
          ~ty)
     (env-add-type '~name ~name)
     '~name))

(defmacro array-type
  [endianness
   signedness
   width
   base-type]
  (cond
   (and (not (= endianness 'little))
        (not (= endianness 'big)))
   (throw (Throwable. (str "Invalid endianness: "
                           endianness)))
   (and (not (= signedness 'signed))
        (not (= signedness 'unsigned)))
   (throw (Throwable. (str "Invalid signedness: "
                           signedness)))
   :default
   `(type/array-type (keyword '~endianness)
                     (keyword '~signedness)
                     ~width
                     ~base-type)))

(def bit
     (type/bit-type))


;;;; Store Language ;;;;
(def *memories* (ref {}))

(defmacro defmem
  "Defines a memory."
  [name type]
  `(do
     (def ~name
          (store/make-memory '~name
                             ~type))
     (env-add-store '~name ~name)
     '~name))

(defmacro defreg
  "Defines a register."
  [name type]
  `(do
     (def ~name
          (store/make-register '~name
                               ~type))
     (env-add-store '~name ~name)
     '~name))

(defmacro defalias
  "Defines an alias."
  ([name store]
     `(do
        (def ~name
             (store/make-alias '~name
                               ~store))
        (env-add-store '~name ~name)
        '~name))
  ([name store begin]
     `(do
        (def ~name
             (store/make-alias '~name
                               ~store
                               ~begin
                               ~begin))
        (env-add-store '~name ~name)
        '~name))  
  ([name store begin end]
     `(do
        (def ~name
             (store/make-alias '~name
                               ~store
                               ~begin
                               ~end))
        (env-add-store '~name ~name)
        '~name)))
