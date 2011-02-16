(ns gist.parser
  (:use [gist.ast]))

(defn parse-error
  [& msg]
  (throw (Exception. (apply str msg))))

;; Forward declaration
(def parse-md-file)

(defn parse-use
  [form]
  (let [file (second form)]
    (parse-md-file file)))

(def k
'(defclass Foo
   (extends Instruction)
   (uses  This That The Other)
   (fetch $inst (array-type bit little unsigned 32))
   (field $disp (array-type bit little unsigned 16) $inst 0 16)
   (method nil effect []
     (seq
      (par (<- x y)
	   (<- q y))
      (par (<- d x)
	   (for [v (array-type bit little unsigned 32)] 0 12
		(seq
		 (par (<- x v)
		      (<- d v)))))))
   ))	   

(defn parse-named-type
  [type]
  (make-named-type type))

(defn parse-array-type
  [type]
  (let [[_ base end sig size] type]
    (make-array-type base end sig size)))

(defn parse-type
  [type]
  (cond
   (and (symbol? type) (= type 'bit)) bit-type
   (nil? type)                        nil-type   
   (symbol? type)                     (parse-named-type type)
   (list?   type)                     (parse-array-type type)
   :else (parse-error "Unknown type: " type)))

(defn parse-fetch-dec
  [dec]
  (let [[_ name type] dec]
    (make-fetch-dec name (parse-type type))))

(defn parse-field-dec
  [dec]
  (let [[_ name type field start length] dec]
    (make-field-dec
     name
     (parse-type type)
     field
     start
     length)))

(defn parse-exp
  [exp]
  exp)

(def parse-stm) ; forward declaration

(defn parse-seq-stm
  [seq]
  (let [stms (rest seq)]
    (loop [astl '()
	   stml stms]
      (if (empty? stml)
	(make-seq-stm (reverse astl))
	(recur (cons (parse-stm (first stml)) astl)
	       (rest stml))))))

(defn parse-par-stm
  [par]
  (let [stms (rest par)]
    (loop [astl '()
	   stml stms]
      (if (empty? stml)
	(make-par-stm (reverse astl))
	(recur (cons (parse-stm (first stml)) astl)
	       (rest stml))))))

(defn parse-if-stm
  [stm]
  (let [[_ cond istm estm] stm]
    (make-if-stm
     (parse-exp cond)
     (parse-stm istm)
     (parse-stm istm))))

(defn parse-for-stm
  [stm]
  (let [[_ vdec from to stmt] stm]
    (make-for-stm
     vdec
     from
     to
     (parse-stm stmt))))

(defn parse-all-stm
  [stm]
  (let [[_ vdec from to stmt] stm]
    (make-all-stm vdec from to stmt)))

(defn parse-ret-stm
  [stm]
  (let [[_ exp] stm]
    (make-ret-stm (parse-exp exp))))

(defn parse-assign-stm
  [stm]
  (let [[_ lhs rhs] stm]
    (make-assign-stm
     (parse-exp lhs)
     (parse-exp rhs))))

(defn parse-stm
  [stm]
  (condp = (first stm)
      '<-   (parse-assign-stm stm)
      'if   (parse-if-stm stm)
      'for  (parse-for-stm stm)
      'all  (parse-all-stm stm)      
      'ret  (parse-ret-stm stm)
      'par  (parse-par-stm stm)
      'seq  (parse-seq-stm stm)
      :else (parse-error "Unknown statement: " stm)))
      
(defn parse-method-dec
  [dec]
  (let [[_ type name formals seq] dec]
    (make-method-dec
     name
     (parse-type type)
     formals
     (parse-seq-stm seq))))

(defn parse-class-dec
  [dec]
  (condp = (first dec)
      'fetch  (parse-fetch-dec  dec)
      'field  (parse-field-dec  dec)
      'method (parse-method-dec dec)
      (parse-error "Unknown class declaration: " (first dec))))
    
(defn parse-class-body
  [body]
  (loop [ast  '()
	 decs body]
    (if (empty? decs)
      (reverse ast)
      (recur (cons (parse-class-dec (first decs)) ast)
	     (rest decs)))))

(defn parse-class
  [form]
  (let [name   (second form)
	super  (second (nth form 2))
	mixins (rest   (nth form 3))
	decs   (parse-class-body (drop 4 form))]
    (make-class-dec name super mixins decs)))

(defn parse-mixin
  [form]
  'mixin)

(defn parse-top-dec
  [form]
  (condp = (first form)
      'use      (parse-use   form)
      'defclass (parse-class form)
      'defmixin (parse-mixin form)
      (parse-error "Unknown top declaration form: " (first form))))

(defn parse-md-file
  [f]
  (with-open [fr (java.io.FileReader. f)
              pb (java.io.PushbackReader. fr)]
    (loop [decs  []
           value (read pb false nil)]
      (if (nil? value)
        decs
        (recur (cons (parse-top-dec value) decs)
               (read pb false nil))))))

