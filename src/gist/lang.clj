(ns #^{:doc    "A library for defining GIST instructions."
       :author "Tim Richards <tim.d.richards@gmail.com>"}
  gist.lang)

(def *machines*        (ref {}))
(def *current-machine* (ref nil))

(defn machine-exists?
  [name]
  (contains? @*machines* name))

(defn in-machine?
  []
  (not (nil? @*current-machine*)))

(defn set-machine
  [name machine]
  (dosync
   (ref-set *machines*
            (assoc @*machines* name
                   machine))))

(defn new-machine
  [name]
  (set-machine name {:name name
                     :insts {}
                     :stores {}}))

(defn lookup-machine
  [name]
  (@*machines* name))

(defn lookup-inst
  [name]
  ((current-machine) name))

(defn current-machine
  []
  (lookup-machine @*current-machine*))

(defn update-inst
  [name enc sem]
  (let [machi (current-machine)
        insts (:insts machi)
        instx (assoc insts name
                     {:name      name
                      :encoding  enc
                      :semantics sem})
        machx (assoc machi :insts instx)]
    (set-machine @*current-machine* machx)))

(defmacro machine
  [name]
  `(dosync
    (ref-set *current-machine* '~name)
    (when (not (machine-exists? '~name))
      (new-machine '~name))
    nil))

(defmacro definst
  [name enc sem]
  (let [qe (->> (partition 2 enc)
                (map #(list `(quote ~(first %)) (second %)))
                (reduce concat)
                (cons 'list))]
    `(if (in-machine?)
       (let [e# (apply assoc {} ~qe)
             s# '~sem]
         (update-inst '~name e# s#)
         nil)
       (throw (Exception. "current machine is not defined.")))))       

(defn stores
  []
  ((current-machine) :stores))

(defn add-store
  [name store]
  (let [stores  (stores)
        storesx (assoc stores name store)
        machine (assoc (current-machine) :stores storesx)]
    (set-machine @*current-machine* machine)))

(defn make-store
  [name size type]
  {:name name
   :size size
   :type type})

(defmacro defstore
  [name size type]
  `(add-store '~name
      (make-store '~name '~size ~type)))

(defmacro array-type
  "Creates a new type with endianness e, signedness s, and width w."
  [e s w]
  `{:endian '~e
    :signed '~s
    :width  ~w})

(defn load-machine
  "Loads a GIST machine description from file f."
  [f]
  (let [oldns (ns-name *ns*)]
    (in-ns 'gist.lang)
    (println (format "Loading machine description file %s..." f))
    (load-file f)
    (in-ns oldns)
    nil))
