(ns #^{:doc    "A library for defining GIST instructions."
       :author ["Tim Richards <richards@cs.umass.edu>",
                "Elisabeth Baseman <ebaseman@cs.umass.edu>"]
       }
  gist.lang)

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
         (let [cargs#  (check-args '~op args#)]
            `(make-op '~'~op '~cargs#)))

       ; This defines the predication function.
       (defn ~pname
         [t#]
         (= (getop t#) '~op)) 
     )))       

;;;; Op Definitions ;;;;

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
