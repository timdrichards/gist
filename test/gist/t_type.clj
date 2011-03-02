(ns gist.t-type
  (:use midje.sweet
        clojure.test
        gist.type))

(facts "about integer type checking"
 (get-type (type-op '(add 4 1)     {})) => int-type?)

(facts "about float type checking"
 (get-type (type-op '(add 4.0 1.0) {})) => float-type?)

(facts "about parameter type checking"
 (get-type (type-op '(add &imm 1)  {})) => error-type?
 (let [e {'&imm (array-type 'little 'signed 32)}]
   (get-type (type-op '(add &imm 1) e)) => array-type?))




 