(ns gist.cisl)

;; protocol - HelloAll
(defprotocol HelloAll
  (hello-all [this] "ASTs that say hello"))

(extend-type GAct
  HelloAll
  (hello-all [this] (hello this)))

(extend-type Seq
  HelloAll
  (hello-all [this]
     (str (hello this) ", "
          (hello-all (:gacts this)))))

(extend-type Instruction
  HelloAll
  (hello-all [this]
     (str (hello this) ", "
          (hello-all (:semantics this)))))


;; protocol - Hello
(defprotocol Hello
  (hello [this] "AST nodes that say hello"))

(extend-type GAct
  Hello 
  (hello [this] "Hello from GAct"))

(extend-type Seq
  Hello 
  (hello [this] "Hello from Seq"))

(extend-type Instruction
  Hello 
  (hello [this] (str "Hello from Instruction [" (:name this) "]")))
