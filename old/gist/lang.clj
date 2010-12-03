(ns gist.lang
  (:use gist.tree
        gist.types))

(defmacro <-
  [lhs rhs]
  `(make-asn '~lhs '~rhs))

(defmacro if
  [cond stm]
  `(make-if cond ~stm))

(defmacro par
  [& gacts]
  `(make-par (list ~@gacts)))

(defmacro seq
  [& pars]
  `(make-seq ~@pars))

(defmacro instruction
  [name params seq]
  (let [vars  (map first  (partition 2 params))
        types (cons 'list (map second (partition 2 params)))]
    `(make-instruction '~name (zipmap '~vars ~types) ~seq)))

(defmacro store
  [name kind & accs]
  `(make-store '~name '~kind ~@accs))

(defmacro array
  [base sig end sz]
  (let [s (if (= sig 'u) :unsigned :signed)
        e (if (= end 'l) :little   :big)]
    `(make-array-type ~base ~s ~e ~sz)))
