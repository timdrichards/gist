;; 32-bit immediates
(defield cond 28 31 0 P)
(defield opcode 21 24 0 O)
(defield s 20 20 0 C)
(defield Rn 16 19 0 C)
(defield Rd 12 15 0 C)
(defield rot-imm 8 11 0 C)
(defield imm8 0 7 0 C)

;; Immediate shifts
(defield shift-imm 7 11 0 C)
(defield shift 5 6 0 C)
 
;; Register shifts
(defield Rs 8 11 0 C)
(defield Rm 0 3 0 C)

(defrule pred
  [inst]
  ([enc (PRED 0) $1]
   [asm $1 "eq"]
   [sem (if Z $1)])
  ([enc (PRED 1) $1]
   [asm $1 "ne"]
   [sem (if (not Z) $1)])
  ([enc (PRED 2) $1]
   [asm $1 "cs"]
   [sem (if C $1)])
  ([enc (PRED 3) $1]
   [asm $1 "cc"]
   [sem (if (not C) $1)])
  ([enc (PRED 14) $1]
   [asm $1]
   [sem (if true $1)]))

(defrule inst
  [data]
  (:pass))

(defrule data
  [s reg reg shiftop]
  ([enc (S 1) (DPFLD 0) (OP 5) $1 $2 $3 $4]
   [asm "adc" $1 $2 "," $3 "," shiftop]
   [sem (par (asn $2 (add $3 $4 C))
             (asn N  (add $3 $4 C))
             (asn Z  (add $3 $4 C))
             (asn C  (add $3 $4 C))
             (asn V  (add $3 $4 C)))])
  
  [s reg reg shiftop]
  ([enc (S 0) (DPFLD 0) (OP 5) $1 $2 $3 $4]
   [asm "adc" $1 $2 "," $3 "," shiftop]
   [sem (par (asn $2 (add $3 $4 C)) $s)]))

(defrule shiftop
  [imm8]
  (:pass)
  )

(defrule imm8
  [(imm8 %i8) (ROT4 $1)]
  (emit-imm8 %i8)
  [(imm8 0)

(defrule imm8
  ([(%i8 $1)]           $1)
  ([(%i8 0)]            0)
  ([(%i8 $1) (ROT8 0)]  $1)
  ([(%i8 $1) (ROT8 $2)] (ror $1 (mul $2 2))))
   
  
(defn emit-imm8-asm
  [imm]
  (<< "#~{imm}"))

(defn emit-reg-asm
  [reg]
  (<< "R~{reg}"))

(defn emit-shli-asm
  [reg shl]
  (<< "R~{reg}, LSL #~{shl}"))

