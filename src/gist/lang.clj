(ns #^{:doc    "A library for defining GIST instructions."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
    gist.lang)

(defmacro type
  "Creates a new type with endianness e, signedness s, and width w."
  [e s w]
  `{:endian '~e
    :signed '~s
    :width  ~w})

(defn param
  "Creates a new parameter with name n and type t."
  [n t]
  {:name n
   :type t})

(defn inst
  "Creates a new instruction with name n, parameters p, and semantics s."
  [n p s]
  {:name n
   :params p
   :semantics s})

;; A machine description is represented by three primary environments:
;;   types   defined types
;;   params  defined parameters
;;   insts   defined instructions
(def types  (atom {}))
(def params (atom {}))
(def insts  (atom {}))

(defn add-type
  "Add the type t named n to the type environment."
  [n t]
  (reset! types (conj @types {n t})))

(defn lookup-type
  "Return the type with name n from the type environment."
  [n]
  (@types n))

(defn add-param
  "Add the parameter named n with type t to the parameter environment."
  [n t]
  (reset! params (conj @params {n (param n t)})))

(defn lookup-param
  "Return the parameter with name n from the parameter environment."
  [n]
  (@params n))

(defn add-inst
  "Add the instruction named n with parameters p and semantics s
   to the instruction environment."
  [n p s]
  (reset! insts (conj @insts {n (inst n p s)})))

(defn lookup-inst
  "Return the instruction with name n from the instruction environment."
  [n]
  (@insts n))

(defmacro deftype
  "Defines a named type n with type t in the type environment."
  [n t]
  `(add-type '~n ~t))

(defmacro defparam
  "Defines a parameter named n with type t in the parameter environment."
  [n t]
  (cond
   (symbol? t) `(add-param '~n (lookup-type '~t))
   :else       `(add-param '~n ~t)))

(def *gist-ops* (atom {}))

(defn add-gist-op
  [n d]
  (reset! *gist-ops*
          (conj @*gist-ops*
                {n d})))

(defn supported-ops
  []
  (map #(get % 0) @*gist-ops*))

(defmacro defop
  "Creates a GIST operator named n constrained to p parameters
  with the doc string d."
  [p n d]
  (do
    (add-gist-op n d)
  `(defmacro ~n
     ~d
     [& args#]
     (let [a# (map (fn [x#] (if (symbol? x#) ; quote symbols in arg list
                            (list 'quote x#)
                            x#)) args#)
           b# '~n
           c# ~p]
       `(if (= (count '~a#) ~c#)
          (list '~b# ~@a#)
          (throw (Exception.
                  (str "Invalid number of arguments: GIST "
                       '~b# " operator - expected " ~c#))))))))

;;;; Definition of GIST operators ;;;;

;; fixed-point binary operations
(defop 2 add
  "GIST add operation.")
(defop 3 addc
  "GIST add with carry operation.")
(defop 2 sub
  "GIST subtract operation.")
(defop 2 mul
  "GIST multiplication operation.")
(defop 2 div
  "GIST division operation.")
(defop 2 band
  "GIST bitwise and operation.")
(defop 2 bord
  "GIST bitwise or operation.")
(defop 2 bxor
  "GIST bitwise xor operation.")
(defop 2 brsz
  "GIST back resize operation.
   New bits are set to 1.")
(defop 2 brsz0
  "GIST back resize operation.
   New bits are set to 0.")
(defop 2 frsz
  "GIST front resize operation.
   New bits are set to 1.")
(defop 2 frsz0
  "GIST front resize operation.
   New bits are set to 0.")
(defop 2 conc
  "GIST catenate operation.
   Operand 1 is catenated to operand 2.")
(defop 2 eq
  "GIST equals comparison operation.
   Returns true if bits are identical.")
(defop 2 ne
  "GIST not equals comparison operation.
   Returns true if bits are not identical")
(defop 2 gt
  "GIST greater-than comparison operation.
   Returns true if operand 1 is greater than
   operand 2.")
(defop 2 lt
  "GIST less-than comparison operation.
   Returns true if operand 1 is less than
   operand 2.")
(defop 2 le
  "GIST less-than or equal to comparison operation.
   Returns true if operand 1 is less than
   or equal to operand 2.")
(defop 2 ge
  "GIST greater-than or equal to comparison operation.
   Returns true if operand 1 is greater than
   or equal to operand 2.")
(defop 2 land
  "GIST logical and operation.")
(defop 2 lor
  "GIST logical or operation.")
(defop 2 lxor
  "GIST logical xor operation.")
(defop 2 ror
  "GIST rotate right operation.")
(defop 2 rol
  "GIST rotate left operation.")
(defop 2 lsh
  "GIST left shift operation.")
(defop 2 rsh
  "GIST right shift operation.")
(defop 2 rsha
  "GIST right shift arithmetic operation.")
(defop 2 sext
  "GIST sign-extend operation.")
(defop 2 zext
  "GIST zero-extend operation.")

;; fixed-point unary operations
(defop 1 lnot
  "GIST logical not operation.")
(defop 1 dec
  "GIST decrement operation.")
(defop 1 inc
  "GIST increment operation.")
(defop 1 neg
  "GIST negate operation.")
(defop 1 pos
  "GIST positive operation.")
(defop 1 pop0
  "GIST population count zero operation.")
(defop 1 pop1
  "GIST population count one operation.")

;; floating-point binary operations
(defop 2 fadd
  "GIST floating-point add operation.")
(defop 2 fsub
  "GIST floating-point subtract operation.")
(defop 2 fmod
  "GIST floating-point modulus operation.")
(defop 2 fmul
  "GIST floating-point multiplication operation.")
(defop 2 fdiv
  "GIST floating-point division operation.")
(defop 2 feq
  "GIST floating-point equals comparison operation.")
(defop 2 fne
  "GIST floating-point not equals comparison operation.")
(defop 2 fgt
  "GIST floating-point greater-than comparison operation.")
(defop 2 flt
  "GIST floating-point less-than comparison operation.")
(defop 2 fge
  "GIST floating-point greater-than or equal to operation.")
(defop 2 fle
  "GIST floating-point less-than or equal to operation.")
(defop 2 fext
  "GIST floating-point extend operation.")

;; floating-point unary operations
(defop 1 fneg
  "GIST floating-point negate operation.")

(defmacro seq
  "GIST sequence statement."
  [& p]
  `(list (symbol "seq") ~@p))

(defmacro par
  "GIST parallel statement."
  [& p]
  `(list (symbol "par") ~@p))

(defmacro ->
  "GIST guarded effect statement."
  [c a]
  (let [l (if (symbol? c) (list 'quote c) c)
        r (if (symbol? a) (list 'quote a) a)]  
  `(list (symbol "->") ~l ~r)))

(defmacro <-
  "GIST assignment statement."
  [t s]
  (let [l (if (symbol? t) (list 'quote t) t)
        r (if (symbol? s) (list 'quote s) s)]
  `(list (symbol "<-") ~l ~r)))

(defmacro definst
  "Defines a GIST instruction."
  [n p s]
  `(add-inst '~n '~p ~s))

(defn load-machine
  "Loads a GIST machine description from file f."
  [f]
  (in-ns 'gist.lang)
  (binding [params (atom {})
            types  (atom {})
            insts  (atom {})]
    (load-file f)
    {:file    f
     :params  @params
     :types   @types
     :insts   @insts}))