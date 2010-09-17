(ns gist.tool
  (:gen-class))

(defn bad-command
  [cmd]
  (println
   (str "gist: '"
        cmd
        "' is not a gist command. "
        "See gist help")))

(defn do-search
  "gist search src tgt smap"
  [args]
  (if (= (count args) 3)
    (let [[src tgt smap] args]
      (println src)
      (println tgt)
      (println smap))
    (throw (IllegalArgumentException.
            (str "I expected 3 arguments for search. See gist help.")))))

(def help-msg "
usage: gist command

The most commonly used gist commands are:
    search    Search for instruction selector patterns
    help      Display help

See 'gist help command' for more information on a specific command.
")

(def help-search "
Search for instruction selector patterns.
usage: gist search src dst smap

    src       The source instruction descriptions
    tgt       The target instruction descriptions
    smap      The store mapping from source to target
")

(def help-cmd
     {"search" help-search})

(defn do-help
  ([]    (println help-msg))
  ([cmd] (if (contains? help-cmd cmd)
           (println (help-cmd cmd))
           (bad-command cmd))))

(defn -main [& args]
  (try
    (if (= (count args) 0)
      (do-help)
      (let [cmd (first args)]
        (condp = cmd
            "search" (do-search (rest args))
            "help"   (apply do-help (rest args))
            (bad-command cmd))))
    (catch IllegalArgumentException e
      (println (.getMessage e)))))
