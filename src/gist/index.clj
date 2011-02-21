(ns #^{:doc "A library for generating index for GIST trees"
       :author "Prasanna Gautam <prasannagautam@gmail.com>"}
  gist.index)
; this is generated from the list at http://primes.utm.edu/lists/small/1000.txt
; using the code at https://gist.github.com/837492
(def optable {add 3853, par 3833, conc 3863, sext 3877, lor 3881, rol 3889, <- 3851, fsub 3907, pop0 3911, lsh 3917, eq 3919, sub 3923, fadd 3929, fmod 3931, fext 3943, div 3947, rsh 3967, rsha 3989, neg 4001, frsz0 4003, ge 4007, inc 4013, le 4019, fge 4021, mul 4027, seq 3823, fle 4049, feq 4051, ne 4057, brsz 4073, lt 4079, band 4091, addc 4093, -> 3847, zext 4099, lnot 4111, brsz0 4127, frsz 4129, bord 4133, land 4139, fdiv 4153, gt 4157, fneg 4159, pop1 4177, flt 4201, pos 4211, ror 4217, dec 4219, lxor 4229, fne 4231, fgt 4241, fmul 4243, bxor 4253})

(defn ignore? "check if the current item can be ignored"
  [n]
  (and (symbol? n)
       (integer? n)))

(defn hash
  [t]
  (let [op (first t)
	children (rest t)]
    (reduce * ((quote op) optable) (map hash children))))

