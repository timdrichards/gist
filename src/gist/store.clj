(ns #^{:doc    "A library for defining GIST instructions."
       :author ["Tim Richards <richards@cs.umass.edu>",
                "Elisabeth Baseman <ebaseman@cs.umass.edu>"]
       }
  gist.store)

(defmacro defstore
  "Defines a store"
  [name ty]
  `(do
     (def ~name {:kind :store,
                 :name '~name,
                 :type ~ty})))
