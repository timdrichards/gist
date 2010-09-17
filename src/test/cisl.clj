(ns gist.cisl)

; CISL declarations
(defrecord CislDec   [name decs])
(defrecord ClassDec  [name super mixins decs])
(defrecord MixinDec  [name super decs])
(defrecord TypeDec   [name type])
(defrecord VarDec    [name type])
(defrecord RefDec    [name type expr])
(defrecord AliasDec  [name params expr])
(defrecord MethodDec [name spec formals body type])
(defrecord StoreDec  [name decs])
(defrecord IdxMemDec [name type size])
(defrecord AdrMemDec [name type size])

; CISL expressions
(defrecord BinExp    [op lexp rexp])
(defrecord UnExp     [op exp])
(defrecord CallExp   [name args])
(defrecord CastExp   [type exp])
(defrecord IdExp     [name])
(defrecord MemExp    [id])
(defrecord LGetExp   [loc idx])
(defrecord NumExp    [val])
(defrecord RealExp   [val])
(defrecord ArrayExp  [arr idx])
(defrecord RangeExp  [arr hi lo])

; CISL statements
(defrecord SeqStm    [stms])
(defrecord ParStm    [stms])
(defrecord LSetStm   [loc rhs])
(defrecord IfStm     [test ifb elseb])
(defrecord ForStm    [var from to body])

