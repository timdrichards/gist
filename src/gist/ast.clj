(ns gist.ast)

;;;; AST Definitions ;;;;
(defrecord NamedType     [name])
(defrecord ArrayType     [base end sig size])
(defrecord BitType       [])
(defrecord IntType       [])
(defrecord FloatType     [])
(defrecord NilType       [])

;; Declarations
(defrecord ClassDec      [name super mixins decs])
(defrecord MixinDec      [name super decs])
(defrecord MethodDec     [name type formals seq])
(defrecord VarDec        [name type exp])
(defrecord TypeDec       [name type])
(defrecord FetchDec      [name type])
(defrecord FieldDec      [name type field start length])
(defrecord AliasDec      [name store start length])
(defrecord StoreDec      [name kind size ctors])
(defrecord SelectorDec   [name type align])
(defrecord StoreClassDec [name exp])

;; Statements
(defrecord SeqStm    [stms])
(defrecord ParStm    [stms])
(defrecord AssignStm [lhs rhs])
(defrecord IfStm     [cond istm estm])
(defrecord ForStm    [vdec from to stm])
(defrecord AllStm    [vdec from to stm])
(defrecord RetStm    [exp])

;; Expressions
(defrecord BinaryExp     [op lexp rexp])
(defrecord UnaryExp      [op exp])
(defrecord CallExp       [name rands])
(defrecord CastExp       [type exp])
(defrecord StoreClassExp [name])
(defrecord ParamExp      [name])
(defrecord AliasExp      [name])
(defrecord MemExp        [exp ctor idx])
(defrecord IntExp        [val])
(defrecord FloatExp      [val])
(defrecord BoolExp       [val])

;;;; AST Constructors ;;;;
(defn make-named-type
  [name]
  (NamedType. name))

(defn make-array-type
  [base end sig size]
  (ArrayType. base end sig size))

(defn make-class-dec
  [name super mixins decs]
  (ClassDec. name super mixins decs))

(defn make-mixin-dec
  [name super decs]
  (MixinDec. name super decs))

(defn make-fetch-dec
  [name type]
  (FetchDec. name type))

(defn make-field-dec
  [name type field start length]
  (FieldDec. name type field start length))

(defn make-method-dec
  [name type formals seq]
  (MethodDec. name type formals seq))

(defn make-seq-stm
  [stms]
  (SeqStm. stms))

(defn make-par-stm
  [stms]
  (ParStm. stms))

(defn make-assign-stm
  [lhs rhs]
  (AssignStm. lhs rhs))

(defn make-if-stm
  [cond istm estm]
  (IfStm. cond istm estm))

(defn make-for-stm
  [vdec from to stm]
  (ForStm. vdec from to stm))

(defn make-all-stm
  [vdec from to stm]
  (AllStm. vdec from to stm))

(defn make-ret-stm
  [exp]
  (RetStm. exp))

;;;; Global Definitions ;;;;
  
;; Type Definitions
(def nil-type (NilType.))
(def bit-type (BitType.))

(defmacro array-type
  [base end sig size]
  `(ArrayType. (NamedType. '~base) (keyword '~end) (keyword '~sig) ~size))
  
(defmacro dectype
  [name type]
  `(def ~name (TypeDec. '~name ~type)))

;; Standard type declarations
(dectype bit  bit-type)
(dectype ui64 (array-type bit little unsigned 64))
(dectype si64 (array-type bit little signed   64))
(dectype ui32 (array-type bit little unsigned 32))
(dectype si32 (array-type bit little signed   32))
(dectype ui16 (array-type bit little unsigned 16))
(dectype si16 (array-type bit little signed   16))
(dectype ui8  (array-type bit little unsigned 8 ))
(dectype si8  (array-type bit little signed   8 ))

;; Value Declarations
(def True  (ValDec. 'true  (NamedType. 'bit) (BoolExp. true)))
(def False (ValDec. 'false (NamedType. 'bit) (BoolExp. false)))

