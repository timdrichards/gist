(ns gist.parse
  (:use gist.tree gist.types)
  (:import gist.types.ArrayType
           gist.types.BitType)
  (:import gist.tree.Instruction
           gist.tree.InstParams
           gist.tree.Seq
           gist.tree.Par
           gist.tree.If
           gist.tree.Asn
           gist.tree.MRef
           gist.tree.BoolConst
           gist.tree.IntConst
           gist.tree.Param
           gist.tree.Op
           gist.tree.SemError))

;; forward reference
(defn parse-exp)

;; definition of all the operators
(def ops #{:add   :sub   :mul   :mod   :div   :band
           :bord  :bxor  :brsz  :brsz0 :frsz  :frsz0
           :conc  :eq    :ne    :gt    :lt    :le
           :ge    :lnot  :land  :lor   :lxor  :rotr
           :rotl  :lsh   :rhs   :rsha  :sext  :zext
           :fadd  :fsub  :fmod  :fmul  :fdiv  :feq
           :fne   :fgt   :flt   :fge   :fle   :fext})

(defn mref?
  "Return true if x is a memory reference"
  [x]
  (cond
   (symbol? x) (= \% (first (str x)))
   (list?   x) (mref? (first x))
   :else       false))
        
(defn param?
  "Return true if x is a parameter"
  [x]
  (if (symbol? x)
    (= \$ (->> x str first))
    false))

;; parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-op
  [t]
  (let [op    (keyword (nth  t 0))
        rands (rest t  )]
    (if (contains? ops op)
      (make-op op (map parse-exp rands))
      (make-error (str "Unknown operator: " op)))))

(defn parse-int
  [t]
  (make-iconst t))

(defn parse-bool
  [t]
  (make-bconst t))

(defn parse-mref
  [t]
  (if (symbol? t)
    (make-mref t :no-index)
    (let [name (nth t 0)
          idx  (nth t 1)]
      (make-mref name
                 (parse-exp idx)))))

(defn parse-param
  [t]
  (make-param t))

(defn parse-exp
  [t]
  (cond (integer? t) (parse-int   t)
        (= 'true  t) (parse-bool  t)
        (mref?    t) (parse-mref  t)
        (param?   t) (parse-param t)
        :else        (parse-op    t)))

(defn parse-asn
  [t]
  (let [lhs (nth t 1)
        rhs (nth t 2)]
    (make-asn (parse-exp lhs)
              (parse-exp rhs))))

(defn parse-if
  [t]
  (let [cond (nth t 1)
        asn  (nth t 2)]
    (make-if (parse-exp cond)
             (parse-asn asn))))

(defn parse-par
  [t]
  (let [ifs (rest t)]
    (make-par (map parse-if ifs))))

(defn parse-seq
  [t]
  (let [pars (rest t)]
    (make-seq (map parse-par pars))))

(defn array-sexp?
  [x]
  (if (list? x)
    (and (= 5      (count x))
         (= 'array (first x)))))

(defn bit-sexp?
  [x]
  (= 'bit x))

(defn parse-array-type
  [t]
  (let [base (nth t 1)
        sign (nth t 2)
        endi (nth t 3)
        size (nth t 4)
        sigt {'u :unsigned 's :signed}
        endt {'l :little   'b :big}]
    (make-array-type
     base
     (sigt sign)
     (endt endi)
     size)))

(defn parse-type
  [t]
  (cond
   (bit-sexp?   t) bit-type
   (array-sexp? t) (parse-array-type t)
   :else           (make-error (str "Unknown type: " t))))

(defn parse-inst-params
  [t]
  (defn make-param-map
    [e env]
    (if (empty? e)
      env
      (let [dec   (first e)
            param (keyword (first dec))
            type  (parse-type (second dec))
            decs  (rest  e)]
        (conj (make-param-map decs env) {param type}))))
  (make-inst-params (make-param-map (partition 2 t) {})))

(defn parse-instruction
  [t]
  (let [name   (nth t 1)
        params (nth t 2)
        seq    (nth t 3)]
    (make-instruction name
                      (parse-inst-params params)
                      (parse-seq seq))))

(defn parse
  [t]
  (let [op (first t)]
    (cond (= op 'instruction) (parse-instruction t)
          :else :invalid-tree)))

;; unparsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Unparse
  "Converts a tree AST into a list"
  (unparse [this] "Unparses a Gist AST into a list"))

(extend-type ArrayType
  Unparse
  (unparse
   [this]
   (let [base (:base this)
         sig  (:sig  this)
         end  (:end  this)
         size (:size this)
         sigt {:signed 's :unsigned 'u}
         endt {:little 'l :big      'b}]
     (list 'array base
           (sig sigt)
           (end endt)
           size))))

(extend-type BitType
  Unparse
  (unparse
   [this]
   'bit))

(extend-type Instruction
  Unparse
  (unparse
   [this]
   (list 'instruction
         (:name this)
         (unparse (:params this))
         (unparse (:seq this)))))

(extend-type InstParams
  Unparse
  (unparse
   [this]
   (let [params (keys (:decs this))
         types  (vals (:decs this))]
     (vec (interleave
           (map #(->> % name symbol) params)
           (map unparse types))))))

(extend-type Seq
  Unparse
  (unparse
   [this]
   (let [pars (:pars this)]
     (cons 'seq (map unparse pars)))))
  
(extend-type Par
  Unparse
  (unparse
   [this]
   (let [asns (:asns this)]
     (cons 'par (map unparse asns)))))

(extend-type If
  Unparse
  (unparse
   [this]
   (let [cond (:cond this)
         stm  (:stm  this)]
     (list 'if
           (unparse cond)
           (unparse stm)))))
           
(extend-type Asn
  Unparse
  (unparse
   [this]
   (let [lhs (:lhs this)
         rhs (:rhs this)]
     (list '<-
           (unparse lhs)
           (unparse rhs)))))

(extend-type MRef
  Unparse
  (unparse
   [this]
   (let [name (:name this)
         idx  (:idx  this)]
     (if (= idx :no-index)
       name
       (list name
             (unparse idx))))))

(extend-type BoolConst
  Unparse
  (unparse
   [this]
   (:val this)))

(extend-type IntConst
  Unparse
  (unparse
   [this]
   (:val this)))

(extend-type Param
  Unparse
  (unparse
   [this]
   (symbol (:name this))))

(extend-type Op
  Unparse
  (unparse
   [this]
   (let [op    (->> this :op name symbol)
         rands (:rands this)]
     (cons op (map unparse rands)))))

(extend-type SemError
  Unparse
  (unparse
   [this]
   (list 'error (:msg this))))

;; examples ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def iconst
     '(instruction iconst
        [$i (array bit u l 32)]
        (seq
         (par (if true (<- %SP (add %SP 1))))
         (par (if true (<- (%S %SP) $i))))))

(def example-1
     '(instruction example-1
        [$disp (array bit u l 32)
         $imm8 (array bit u l 32)]            
        (seq
         (par (if true (<- (%R 3) (add $disp 5)))
              (if true (<- (%R 4) (sub 9 $imm8)))))))

(defn works?
  []
  (and (= iconst    (unparse (parse iconst)))
       (= example-1 (unparse (parse example-1)))))