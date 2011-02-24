(deftype lu4 (type little unsigned 4))
(deftype lu1 (type little unsigned 1))
(deftype lu8 (type little unsigned 8))

(defparam rd   lu4)
(defparam rn   lu4)
(defparam rs   lu4)
(defparam imm8 lu8)
(defparam s    lu1)

(definst adci
  [rn rd imm8]
  (seq
   (par (-> true (<- %R:rd (addc %R:rn %I:imm8 $C)))
        (-> true (<- $C (addc %R:rn %I:imm8 $C))))))

(definst adcis
  [rn rd imm8 s]
  (seq
   (par (<- %R:rd (addc %R:rn %I:imm8 $C))
        (<- $N (addc %R:rn %I:imm8 $C))
        (<- $Z (addc %R:rn %I:imm8 $C))
        (<- $C (addc %R:rn %I:imm8 $C))
        (<- $V (addc %R:rn %I:imm8 $C)))))