(ns gist.types)

(defrecord ArrayType [base end sig size])
(defrecord BitType   [])
(defrecord NoType    [])

(defn make-array-type
  [base sign endi size]
  (ArrayType. base endi sign size))

(def no-type  (NoType.))
(def bit-type (BitType.))
(def bu32     (make-array-type bit-type :big    :unsigned   32))
(def bs32     (make-array-type bit-type :big    :signed     32))
(def lu32     (make-array-type bit-type :little :unsigned   32))
(def ls32     (make-array-type bit-type :little :signed     32))

(defprotocol Typed
  "Allows a CISL AST node to type resolve"
  (resolve-types [this] "Resolves types in a cisl tree"))

