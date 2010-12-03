(ns gist.machine)

(defn make-machine
  ([name]
     {:name    name
      :stores  {}
      :aliases {}
      :classes {}
      :insts   {}})
  ([name stores aliases classes insts]
     {:name    name
      :stores  stores
      :aliases aliases
      :classes classes
      :insts   insts}))

(defn machine-name
  [m]
  (:name m))

(defn machine-stores
  [m]
  (:stores m))

(defn machine-aliases
  [m]
  (:aliases m))

(defn machine-classes
  [m]
  (:classes m))

(defn machine-insts
  [m]
  (:insts m))

(defn machine-add-store
  [mac name store]
  (make-machine
   (machine-name mac)
   (conj (machine-stores mac)
         {name store})
   (machine-aliases mac)
   (machine-classes mac)
   (machine-insts mac)))

(defn machine-add-inst
  [mac name inst]
  (make-machine
   (machine-name mac)
   (machine-stores mac)
   (machine-aliases mac)
   (machine-classes mac)
   (conj (machine-insts mac)
         {name inst})))

(defn machine-add-alias
  [mac name alias]
  (make-machine
   (machine-name mac)
   (machine-stores mac)
   (conj (machine-aliases mac)
         {name alias})
   (machine-classes mac)   
   (machine-insts mac)))

(defn machine-add-class
  [mac name class]
  (make-machine
   (machine-name mac)
   (machine-stores mac)
   (machine-aliases mac)
   (conj (machine-classes mac)
         {name class})
   (machine-insts mac)))

(defn make-store-accs
  [accs]
  (if (empty? accs)
    {}
    (let [[name type] (first accs)]
      (conj {name type}
            (make-store-accs (rest accs))))))

(defn make-store
  [name kind size accs]
  {:name name
   :kind kind
   :size size
   :accs (make-store-accs accs)})

(defn store-name
  [st]
  (:name st))

(defn store-kind
  [st]
  (:kind st))

(defn store-size
  [st]
  (:size st))

(defn store-accs
  [st]
  (:accs st))

(defn make-alias
  [name ref from to]
  {:name name
   :ref  ref
   :from from
   :to   to})

(defn alias-name
  [a]
  (:name a))

(defn alias-ref
  [a]
  (:ref a))

(defn alias-from
  [a]
  (:from a))

(defn alias-to
  [a]
  (:to a))

(defn make-class
  [name class-exp]
  (defn eval
    [e]
    (if (not (list? e))
      e
      (condp = (first e)
          'any (set (map eval (rest e))))))
  {:name  name
   :class (eval class-exp)})

(defn class-name
  [c]
  (:name c))

(defn class-set
  [c]
  (:class c))

(defn make-inst
  [name params tree]
  {:name   name
   :params (apply hash-map params)
   :tree   tree})

(defn parse-inst
  [mac exp]
  (let [[name params tree] (rest exp)
        inst (make-inst name params tree)]
    (machine-add-inst mac name inst)))

(defn parse-class
  [mac exp]
  (let [[name exp] (rest exp)
        class (make-class name exp)]
    (machine-add-class mac name class)))

(defn parse-alias
  [mac exp]
  (let [[name ref from to] (rest exp)
        alias (make-alias name ref from to)]
    (machine-add-alias mac name alias)))

(defn parse-store
  [mac exp]
  (let [[name kind size & accs] (rest exp)
        store (make-store name kind size accs)]
    (machine-add-store mac name store)))

(defn parse-list
  [mac exp]
  (condp = (first exp)
      'store (parse-store mac exp)
      'alias (parse-alias mac exp)
      'class (parse-class mac exp)
      'instruction (parse-inst mac exp)
      "parse-list: unknown expression"))

(defn parse
  [mac exp]
  (if (list? exp)
    (parse-list mac exp)
    "parse: unknown expression"))

(defn load-machine
  "Returns a machine object parsed from the file f."
  [f]
  (defn parse-name
    [f]
    (let [file (last (.split f "/"))
          name (.substring file 0 (.indexOf file ".gast"))]
      name))
  
  (with-open [fr (java.io.FileReader. f)
              pb (java.io.PushbackReader. fr)]
    (loop [mac   (make-machine (parse-name f))
           exp   (read pb false nil)]
      (if (nil? exp)
        mac
        (recur
         (parse mac exp)
         (read pb false nil))))))

(defn load-machine-str
  "Returns a machine object parsed from the string s."
  [s]
  (with-open [fr (java.io.StringReader. s)
              pb (java.io.PushbackReader. fr)]
    (loop [mac   (make-machine "string")
           exp   (read pb false nil)]
      (if (nil? exp)
        mac
        (recur
         (parse mac exp)
         (read pb false nil))))))

(def mac
     (load-machine-str
 "(store M addressed *
   [word (array bit u l 32)]
   [half (array bit u l 16)]
   [byte (array bit u l 8 )])

(store SP indexed 1
   [word (array bit u l 32)])

(store R  indexed 32
       [word (array bit u l 32)])

(alias EAX R 0 0)
(alias EBX R 3 3)
(alias ECX R 1 1)
(alias EDX R 2 2)
(alias ESI R 6 6)
(alias EDI R 7 7)
(alias EBP R 5 5)
(alias ESP R 4 4)

(alias AX EAX 0 15)
(alias BX EBX 0 15)
(alias CX ECX 0 15)
(alias DX EDX 0 15)

(alias AL AX 0 7)
(alias AH AX 8 15)
(alias BL BX 0 7)
(alias BH BX 8 15)
(alias CL CX 0 7)
(alias CH CX 8 15)
(alias DL DX 0 7)
(alias DH DX 8 15)

(class GR32 (any %EAX %EBX %ECX %EDX))
(class GR16 (any %AX  %BX  %CX  %DX))
(class GR8H (any %AH  %BH  %CH  %DH))
(class GR8L (any %AL  %BL  %CL  %DL))

(instruction example-1
   [$disp (array bit u l 32)
    $imm8 (array bit u l 32)]            
   (seq
    (par (if true (<- %GR32 (add $disp 5)))
         (if true (<- %GR32 (sub 9 $imm8))))))
"))
