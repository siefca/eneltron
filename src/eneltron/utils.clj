(ns ^{:doc    "Eneltron library, utility functions."
      :author "Paweł Wilk"}
    
    eneltron.utils)

;; Conversions.
;;

(defn ensure-sequential
  "Ensures that the given argument is sequential. If it's not, then it creates
  a list containing its value as the only element."
  [s]
  (if (sequential? s) s (list s)))

;; Vars.
;;

(defmacro defonce-var
  "Works in the same way like defonce but always returns a Var object."
  [name expr]
  `(let [v# (def ~name)]
     (if (.hasRoot v#)
       v#
       (def ~name ~expr))))

;; Function arguments tweaks.
;;

(defn- fargs-count-clj
  "Uses Var's :arglists to determine number of arguments the function bound to
  it takes."
  [varobj]
  (when-some [larity (-> varobj meta :arglists first)]
    (let [count-all (count larity)
          regulargs (take-while (complement #{'&}) larity)
          count-reg (count regulargs)]
      [count-reg (> count-all count-reg) :clj])))

(defn- fargs-count-jvm
  "Uses JVM reflection calls to determine number of arguments the given function
  takes."
  [f]
  {:pre [(ifn? f)]}
  (let [meths     (.getDeclaredMethods (class f))
        variaargs (filter #(= "doInvoke" (.getName %)) meths)
        regulargs (filter #(= "invoke"   (.getName %)) meths)
        regular-m (map    #(alength      (.getParameterTypes %)) regulargs)
        variaar-m (map    #(alength      (.getParameterTypes %)) variaargs)
        total-cnt (+ (reduce max 0 regular-m) (dec (reduce max 1 variaar-m)))]
    [total-cnt (not (empty? variaar-m)) :jvm])) 

(defmacro fargs-count
  "Determines the number of arguments that function f takes with its most wide
  arity. Returns a vector in which the first element is a number of arguments,
  the second is true if function takes variadic arguments (false otherwise) and
  the third is either :cjl (if metadata were used to determine arity)
  or :jvm (if reflection methods were used)."
  [f]
  `(let [fr# (resolve (quote ~f))]
     (or (when (var? fr#) (fargs-count-clj fr#))
         (fargs-count-jvm ~f))))

(defmacro fargs
  "Wraps function that takes some number of args into a function that takes
  variable number of args and always passes the given number of them when
  called.
  
  If the number is not given, it will be determined using metadata (if the given
  object is a symbol bound to a Var) or JVM reflection calls to anonymous class
  representing a function object.
  
  If the declared number of arguments is lower than the number of arguments
  passed, then it ignores exceeding ones and not passes them.
  
  When the declared number of arguments that original function takes is higher
  than the number of arguments that are really passed to it, it pads extra
  arguments with nil values.
  
  When the :varargs option was given (or the variadic function was detected when
  no number of arguments was explicitly passed), all the arguments received by
  a wrapper will be passed as they are, regardless of their count. The
  obligatory arguments will be padded with nil values up to their required
  count if necessary."
  ([f]
   `(let [[nr# va# _] (fargs-count ~f)]
      (fargs ~f nr# va#)))
  ([f opt]
   `(let [o# ~opt]
      (if (= o# :varargs)
        (fargs ~f (first (fargs-count ~f)) true)
        (fargs ~f o# false))))
  ([f nr varargs]
   `(let [f# ~f nr# ~nr]
      (if ~varargs
        (fn [& args#] (apply f# (concat args# (repeat (- nr# (count args#)) nil))))
        (fn [& args#] (apply f# (take nr# (concat args# (repeat nil)))))))))

