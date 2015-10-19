(ns eneltron.utils
  ^{:doc "Eneltron library – utility functions."
    :author "Paweł Wilk"})

(defmacro defonce-var
  "Works in the same way like defonce but always returns Var object."
  [name expr]
  `(let [v# (def ~name)]
     (if (.hasRoot v#)
       v#
       (def ~name ~expr))))

