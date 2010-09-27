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
           gist.tree.MemIdx
           gist.tree.BoolConst
           gist.tree.IntConst
           gist.tree.Param
           gist.tree.Op
           gist.tree.SemError
           gist.tree.Store
           gist.tree.Alias
           gist.tree.StClass
           gist.tree.StClassAny))

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

(defn memidx?
  "Return true if x is a memory index"
  [x]
  (cond
   (symbol? x) (= \% (first (str x)))
   (list?   x) (and (memidx? (first x))
                    (or (= (count x) 2)
                        (= (count x) 1)))
   :else       false))

(defn memrng?
  "Return true if x is a memory range"
  [x]
  (cond
   (symbol? x) (= \% (first (str x)))
   (list?   x) (and (memidx? (first x))
                    (= (count x) 3))
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

(defn parse-memidx
  [t]
  (if (symbol? t)
    (make-memidx t :no-index)
    (let [name (nth t 0)
          idx  (nth t 1)]
      (make-memidx name
                   (parse-exp idx)))))

(defn parse-memrng
  [t]
  (let [[name from to] t]
    (make-memrng name from to)))

(defn parse-param
  [t]
  (make-param t))

(defn parse-exp
  [t]
  (cond (integer? t) (parse-int    t)
        (= 'true  t) (parse-bool   t)
        (memidx?  t) (parse-memidx t)
        (memrng?  t) (parse-memrng t)
        (param?   t) (parse-param  t)
        :else        (parse-op     t)))

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

(defn parse-alias
 [t]
 (let [[alias store from to] (rest t)]
   (if to
     (make-alias alias store from to)
     (make-alias alias store from from))))

(defn parse-store-class-exp) ; forward reference

(defn parse-store-class-any
  [t]
  (let [ee (rest t)]
    (make-store-class-any
     (map parse-store-class-exp ee))))

(defn parse-store-class-exp
  [t]
  (cond
   (memidx?  t)       (parse-memidx t)
   (= (first t) 'any) (parse-store-class-any t)))

(defn parse-store-class
  [t]
  (let [name (nth t 1)
        exp  (parse-store-class-exp (nth t 2))]
    (make-store-class name exp)))

(defn parse-store
  [t]
  (defn parse-accs
    [accs]
    (let [pass1 (for [[x y] accs]   
                  [(keyword x)
                   (parse-type y)])
          pass2 (apply hash-map (flatten pass1))] 
      pass2))
  (let [name (nth t 1)
        kind (nth t 2)]
    (if (= kind 'indexed)
      (let [size (nth t 3)
            accs (nthnext t 4)]
        (make-store name kind size
                    (parse-accs accs)))
      (let [accs (nthnext t 3)]
        (make-store name kind
                    (parse-accs accs))))))

(defn parse
  "Returns a gist instruction or :invalid-tree if the
  list t is not a valid instruction definition."
  [t]
  (let [op (first t)]
    (cond (= op 'instruction) (parse-instruction t)
          (= op 'store)       (parse-store t)
          (= op 'alias)       (parse-alias t)
          (= op 'class)       (parse-store-class t)
          :else :invalid-tree)))

(defn parse-desc
  "Returns a list of gist instructions parsed from the
  given gist description file f."
  [f]
  (with-open [fr (java.io.FileReader. f)
              pb (java.io.PushbackReader. fr)]
    (loop [insts []
           value (read pb false nil)]
      (if (nil? value)
        insts
        (recur (cons (parse value) insts)
               (read pb false nil))))))

;; unparsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Unparse
  "Converts a tree AST into a list"
  (unparse [t] "Returns a list representation of the gist instruction tree t."))

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

(extend-type MemIdx
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

(extend-type Store
  Unparse
  (unparse
   [this]
   (let [store (:name this)
         kind  (:kind this)
         size  (:size this)
         accs  (for [[acc type] (:accs this)]
                 [(->> acc name symbol) (unparse type)])]
     (if (= kind 'addressed)
       (concat (list 'store store kind) accs)
       (concat (list 'store store kind size) accs)))))

(extend-type Alias
  Unparse
  (unparse
   [this]
   (let [name  (:name this)
         store (:store this)
         from  (:from this)
         to    (:to this)]
     (if (= from to)
       (list 'alias name from)
       (list 'alias name from to)))))

(extend-type StClass
  Unparse
  (unparse
   [this]
   (let [name (:name this)
         exp  (:exp  this)]
     (list 'class name
           (unparse exp)))))

(extend-type StClassAny
  Unparse
  (unparse
   [this]
   (cons 'any (map unparse (:class-list this)))))

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

(defn test-parse
  []
  (clojure.contrib.pprint/pprint (map unparse (parse-desc "md/ia32.gast"))))
  