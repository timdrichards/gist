(machine arm)

;; types
(def lu4  (array-type little unsigned 4))
(def lu1  (array-type little unsigned 1))
(def lu8  (array-type little unsigned 8))
(def ls8  (array-type little signed   8))
(def ls16 (array-type little signed   16))
(def ls32 (array-type little signed   32))

(defstore R 16 ls32)
(defstore Mb * ls8 )
(defstore Mh * ls16)
(defstore Mw * ls32)

;; (defclass %R R (range 0 15))
;; (defclass %I I 

(definst adci
  [rd lu4, rn lu4, rs lu4, imm8 lu8, s lu1]
  (seq
   (par (-> true (<- %R:rd (addc %R:rn %I:imm8 $C)))
        (-> true (<- $C (addc %R:rn %I:imm8 $C))))))

;; (definst adcis
;;   [rn lu4, rd lu4, imm8 lu8, s lu1]
;;   (seq
;;    (par (<- %R:rd (addc %R:rn %I:imm8 $C))
;;         (<- $N (addc %R:rn %I:imm8 $C))
;;         (<- $Z (addc %R:rn %I:imm8 $C))
;;         (<- $C (addc %R:rn %I:imm8 $C))
;;         (<- $V (addc %R:rn %I:imm8 $C)))))

