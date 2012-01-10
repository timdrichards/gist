(ns #^{:doc    "A library for defining GIST instructions."
       :author ["Tim Richards <richards@cs.umass.edu>",
                "Elisabeth Baseman <ebaseman@cs.umass.edu>"]
       }
  gist.lang)

(defn make-op
  [op args]
  (cons op args))

(defn op?
  [t]
  (and (seq? t)
       (symbol? (first t))))

(defn args
  [t]
  (rest t))

(defn argn
  [t index]
  (nth (args t) index)) 

(defn getop
  [t]
  (first t))

(defn sameop
  [t1 t2]
  (= (getop t1)
     (getop t2)))

(defn isvar?
  [t]
  (if (symbol? t)
    (= \$ (first (seq (str t))))
    false))

(defn var-name
  [v]
  (let [s (seq (str v))  ; to sequence
        n (rest s)]      ; var name
    (symbol (apply str n))))

(defn check-tree
  [t]
  (cond
    (isop?    t) (map check-tree t)
    (integer? t) (make-op 'iconst (list t))
    (isvar?   t) (make-op 'varn (list (var-name t)))
    :default  t))

(defn check-args
  [op args]
  (if (not (or (= op 'iconst)
               (= op 'varn)))
    (map check-tree args)
    args))

(defmacro defop
  [op]
  `(defmacro ~op
     [& args#]
     (let [cargs#  (check-args '~op args#)]
       `(make-op '~'~op '~cargs#))))

;; Define operations:

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
