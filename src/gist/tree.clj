(ns gist.tree)

(defn op
  [t]
  (first t))

(defn children
  [t]
  (next t))

(defn atom?
  [t]
  (not (seq? t)))

(defn op?
  [t]
  (not (atom? t)))

(defn const?
  [t]
  (number? t))

(defn param?
  [t]
  (and (atom? t)
       (= (.charAt (str t) 0) \$)))

(defn mem?
  [t]
  (and (atom? t)
       (= (.charAt (str t) 0) \%)))
