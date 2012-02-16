(ns #^{:doc    "A library for describing GIST types."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.type
  (:use [gist.lang]))

;;;; Type Data Structure Abstraction ;;;;
(defn make-type
  "Returns a type of kind k with info i."
  [k i]
  {:type k :info i})

(defn type-kind
  "Returns the kind of type t.  The kind of a type
   can be:
     (1) :array"
  [t]
  (:type t))

(defn type-info
  "Returns the info of type t.  The info of a type
   includes details such as endianness, signedness,
   bit width, and base type."
  [t]
  (:info t))

;;;; Type Declarations ;;;;

(defn unknown-type
  "Returns an unknown type.  The unknown type is used
   to give a tree node whose type is not yet known."
  [m]
  (make-type :unknown m))

(defn error-type
  "Returns an error type.  The error type is used to
   indicate a type error that occurred in a tree."
  [m]
  (make-type :error m))

(defn seq-type
  "Returns a sequence type."
  []
  (make-type :seq :seq))

(defn par-type
  "Returns a parallel type."
  []
  (make-type :par :par))

(defn geff-type
  "Returns a guarded effect type."
  []
  (make-type :-> :->))

(defn asn-type
  "Returns an assignment type."
  []
  (make-type :<- :<-))

(defn int-type
  "Returns an integer type."
  []
  (make-type :int :int))

(defn float-type
  "Returns a float type with width w."
  [w]
  (make-type
   :float
   {:kind  754
    :width w}))

(defn array-type
  "Returns an array type with endianess e, signedness s,
   and width w."
  [e s w b]
  (make-type
   :array
   {:endian e
    :signed s
    :width  w
    :base   b}))

(defn bit-type
  "Returns a bit type."
  []
  (array-type :none
              :none
              1
              :none))


;;;; Type Predicates ;;;;

(defn istype?
  "Returns true if type is of kind k."
  [t k]
  (= (:type t) k))

(defn error-type?
  "Returns true if t is an error type."
  [t]
  (istype? t :error))

(defn int-type?
  "Returns true if t is an integer type."
  [t]
  (istype? t :int))

(defn bit-type?
  "Returns true if t is a bit type."
  [t]
  (and (istype? t :array)
       (= (:base (type-info t))
          :none)))

(defn array-type?
  "Returns true if t is an array type."
  [t]
  (istype? t :array))

(defn float-type?
  "Returns true if t is a float type."
  [t]
  (istype? t :float))

(defn same-type?
  "Returns true if t1 and t2 are of the same type."
  [t1 t2]
  (= (type-kind t1)
     (type-kind t2)))

(defn set-type
  "Returns a new tree node constructed from n
   with its type set to t."
  [n t]
  (let [m (meta n)]
    (with-meta n
      (assoc m :type t))))

(defn get-type
  "Returns the type of tree t."
  [t]
  (:type (meta t)));

(defn repr
  "Returns the string representation of type t."
  [t]
  (let [i (type-info t)]
    (cond
     (error-type? t)  (str "error-type")
     (int-type?   t)  "int-type"
     (bit-type?   t)  "bit"
     (float-type? t)  "float-type"
     (array-type? t)  (str "array-type<" (:endian i) "," (:signed i)
                           "," (:width i)
                           "," (repr (:base i)) ">")
     :else
     (str t))))

(defmacro defmtype
  [name ty]
  `(do
     (def ~name
          ~ty)))