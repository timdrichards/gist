(ns #^{:doc    "A library for defining GIST instructions."
       :author ["Tim Richards <richards@cs.umass.edu>",
                "Elisabeth Baseman <ebaseman@cs.umass.edu>"]
       }
  gist.store
  (:require [gist.type :as type]))

;;;; Store Data Structure Definitions ;;;;

(defn make-store
  [family name type]
  {:kind   :store
   :family family
   :name   name
   :type   type})

(defn get-family
  [store]
  (:family store))

(defn get-name
  [store]
  (:name store))

(defn get-type
  [store]
  (:type store))

(defn make-memory
  [name type]
  (make-store :memory
              name
              type))

(defn make-register
  [name type]
  (make-store :register
              name
              type))

;;;; Getters/Setters ;;;;
(defn get-store-type
  [store]
  (:type store))

;;;; Store Predicates ;;;;
(defn store?
  [obj]
  (and (map? obj)
       (= (:kind obj) :store)))

(defn memory?
  [obj]
  (and (map? obj)
       (= (:kind obj) :store)
       (= (:family obj) :memory)))

(defn register?
  [obj]
  (and (map? obj)
       (= (:kind obj) :store)
       (= (:family obj) :register)))

;;;; Alias Data Structure Abstraction ;;;;

(defn make-alias
  ([name store]
     (if (store? store)
       {:kind  :alias
        :name  name
        :begin 0
        :end   (type/get-type-width
                (get-store-type store))
        :store store}
       (throw (Throwable. "Invalid store object given to make-alias"))))
  ([name store begin end]
     (if (store? store)
       {:kind  :alias
        :name  name
        :begin begin
        :end   end
        :store store}
       (throw (Throwable. "Invalid store object given to make-alias")))))

;;;; Store Language ;;;;

(defmacro defmem
  "Defines a memory."
  [name type]
  `(do
     (def ~name
          (make-memory '~name
                       ~type))))

(defmacro defreg
  "Defines a register."
  [name type]
  `(do
     (def ~name
          (make-register '~name
                         ~type))))

(defmacro defalias
  "Defines an alias."
  ([name store]
     `(do
        (def ~name
             (make-alias '~name ~store))))
  ([name store begin end]
     `(do
        (def ~name
             (make-alias '~name ~store ~begin ~end)))))
