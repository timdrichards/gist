(ns gist.cisl
  (:use clojure.contrib.str-utils))

; Definitions for types.
(defrecord ArrayType [end sig size])
(defrecord BitType   [])

(defmacro defty
  [name end sig size]
  `(def ~name (ArrayType. '~end '~sig ~size)))

(def bit (BitType.))
(defty bu32 b u 32)
(defty bs32 b s 32)
(defty lu32 l u 32)
(defty ls32 l s 32)
(defty bu16 b u 16)
(defty bs16 b s 16)
(defty lu16 l u 16)
(defty ls16 l s 16)
(defty bu8 b u 8)
(defty bs8 b s 8)
(defty lu8 l u 8)
(defty ls8 l s 8)

; Instruction definitions
(defrecord Instruction [name semantics])
(defrecord Seq [gacts])
(defrecord GAct [guard action])
(defrecord BoolConst [name])
(defrecord IntConst [val])
(defrecord LSet [lhs rhs])
(defrecord LGet [mem acc exp])
(defrecord Op [op rands])
(defrecord Id [name])
(defrecord Param [p])

(defmacro instruction
  [name semantics]
  `(def ~name (Instruction. '~name ~semantics)))

(defmacro sequ
  [& gacts]
  `(Seq. (list ~@gacts)))

(defmacro gact
  [guard action]
  `(GAct. ~guard ~action))

(defmacro lget
  [mem acc exp ty]
  (let [ty# ty]
    `(with-meta (LGet. ~mem ~acc ~exp) ~ty#)))

(defmacro lset
  [lhs rhs]
  `(LSet. ~lhs ~rhs))

(defmacro iconst
  [val ty]
  (let [ty# ty]
    `(with-meta (IntConst. ~val) ~ty#)))

(defmacro sym
  [val]
  `(Id. '~val))

(defmacro param
  [p ty]
  (let [ty# ty]
    `(with-meta (Param. ~p) ~ty#)))

(defmacro ty
  [end sig size]
  `(ArrayType. '~end '~sig ~size))

(def True  (with-meta (BoolConst. true)  bit))
(def False (with-meta (BoolConst. false) bit))

(defmacro add  [x y] `(Op. :add  [~x ~y]))
(defmacro sub  [x y] `(Op. :sub  [~x ~y]))
(defmacro band [x y] `(Op. :band [~x ~y]))
(defmacro bor  [x y] `(Op. :bor  [~x ~y]))
(defmacro bxor [x y] `(Op. :bxor [~x ~y]))
(defmacro div  [x y] `(Op. :div  [~x ~y]))
(defmacro mul  [x y] `(Op. :mul  [~x ~y]))
(defmacro modu [x y] `(Op. :modu [~x ~y]))
(defmacro lsh  [x y] `(Op. :lsh  [~x ~y]))
(defmacro rsh  [x y] `(Op. :rsh  [~x ~y]))
(defmacro rotl [x y] `(Op. :rotl [~x ~y]))
(defmacro rotr [x y] `(Op. :rotr [~x ~y]))
(defmacro neg  [x]   `(Op. :neg  [~x]))
(defmacro pos  [x]   `(Op. :pos  [~x]))
(defmacro pop1 [x y] `(Op. :pop1 [~x ~y]))
(defmacro pop0 [x y] `(Op. :pop0 [~x ~y]))

; to help convert string types to types
(defn str-ty [s]
  (if (= s "bit")
    bit
    (let [e (symbol (first s))
          g (symbol (second s))
          z (Integer. (str-join "" (rest (rest s))))]
      (ty e g z))))

(instruction
 aconst_null
 (sequ
  (gact True
    (lset
     (lget (sym s)
           (sym slot)
           (param (sym spTopOffset) (ty b s 32))
           (ty b s 32))
     (iconst 0 (ty b s 32))))
  (gact True
    (lset
     (lget (sym s)
           (sym slot)
           (param (sym spTopOffset) (ty b s 32))
           (ty b s 32))
     (iconst 0 (ty b s 32))))
  ))