(ns #^{:doc    "A library for defining GIST instructions."
       :author ["Tim Richards <richards@cs.umass.edu>",
                "Elisabeth Baseman <ebaseman@cs.umass.edu>"]
       }
  gist.store)

(defmacro defmem
  "Defines a memory"
  [name ty]
  `(do
     (def ~name {:kind :store
                 :name '~name
                 :type ~ty})))
