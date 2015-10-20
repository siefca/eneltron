(ns ^{:doc    "Eneltron library, tokenization support."
      :author "Paweł Wilk"}
    
    eneltron.tokenizer
  
  (:require [eneltron.utils :refer :all]
            [eneltron.chars :refer :all]))

;; Default options.

(def default-options
  {:squeeze         nil   ; classes to squeeze (sequential coll) or nil/false
   :squeeze-chars   nil   ; characters to squeeze
   :keep-newlines   true  ; whether to keep newlines (true/false/:inherit)
   :tag-quotes      true  ;
   :tag-parens      true  ;
   :unify-quotes    true  ;
   :input-sentences false ; whether to drop final dots
   :trim            true  ; trim spaces from beginnings and ends of sentences
   })

;; Default mappings.

(def conf-seed {:classes {} :rules {} :options default-options})

;; Default token mappings.

(def ^:dynamic *config* conf-seed)

;; Generic configuration operations.
;;

(defn clear-rules
  "Takes token mappings and clears rules section. Returns new map."
  [mtok]
  (assoc mtok :rules {}))

(defn clear-classes
  "Takes token mappings and clears classes section. Returns new map."
  [mtok]
  (assoc mtok :classes {}))

(defn clear-options
  "Takes token mappings and sets options section to defaults. Returns new map."
  [mtok]
  (assoc mtok :options default-options))

(defn get-conf
  "Returns the default tokenizer config (bound to *config* special variable)."
  []
  *config*)

(defn get-conf-section
  "Returns a section of tokenizer configuration. Section should be a keyword
  given as the first argument and configuration should be a map given as the
  second, optional argument. If it's not given, or if its value is nil or false,
  the *config* special variable will be used as a source of configuration.
  
  If a section cannot be found it return an empty map."
  [section & [mtok]]
  ((or mtok *config*) section {}))

(defn- update-section
  "Helper function that updates branch in tokenizer's configuration map using
  reduce. Args: function map section elements-source"
  [sect mtok f src-seq]
  (update-in mtok [sect] #(reduce f %1 src-seq)))

;; Token classes operations.
;;

(defn assoc-char
  "Associates numeric type of character given as the last argument with a token
  class given as the second argument by updating confguration map passed as the
  first argument.
  
  Returns an updated map."
  [mtok token-class ^Character c]
  (assoc-in mtok [:classes (get-char-type-nr c)] token-class))

(defn assoc-chars
  "Associates numeric type of each character from a sequence of characters given
  as a last argument with a token class given as a third argument by updating
  a map passed as a first argument.
    
  Returns an updated map."
  [mtok token-class charseq]
  (update-section :classes mtok
                  #(assoc %1 (get-char-type-nr %2) token-class)
                  (ensure-char-seq charseq)))

(defmacro assoc-chars->
  "Updates a map given as the first argument in a way that it associates each
  numeric type of character from a sequence of characters (or a string) given as
  the last elements of pairs with token classes given as the first elements of
  pairs. with a token class given as a third argument by updating a map passed
  as a first argument.
  
  Returns an updated map."
  [mtok & kvs]
  (let [calls (pmap #(cons 'assoc-chars %1) (partition-all 2 kvs))]
    `(-> ~mtok ~@calls)))

(defmacro defclasses
  "Creates mappings character-type-nr->:token-class. Takes name of dynamic
  variable to define and mappings expressed as pairs, where first element should
  be a character and second a keyword naming its class."
  [name & kvs]
  (let [dyname (with-meta name {:dynamic true})
        calls  (pmap #(cons 'assoc-chars %1) (partition-all 2 kvs))]
    `(def ~dyname (-> conf-seed ~@calls))))

(defn get-class
  "Gets token class for a given character."
  [^Character chr & [mtok]]
  ((get-conf-section :classes mtok) (get-char-type-nr chr)))

;; Token rules operations.
;;

(defn assoc-rule
  "Associates a token rule given as the second argument with a token class given
  as the last argument by updating a map (:token-class->:token-rule) passed as
  the first argument. If there are more arguments, it does that for any
  element (any token class).
  
  Returns an updated map."
  ([mtok token-rule token-classes]
   (if (= :default token-rule)
     (recur mtok token-classes token-rule)
     (let [tclasses (pmap keyword
                          (if (sequential? token-classes)
                            token-classes
                            (list token-classes)))]
       (update-section :rules mtok #(assoc %1 %2 token-rule) tclasses))))
  ([mtok token-rule token-class & other-classes]
   (assoc-rule mtok token-rule (cons token-class other-classes))))

(defmacro defrules
  "Definies new set of rules for token classes by creatnig mappings
  :token-class->:token-rule. Takes name of dynamic variable to define (if not
  defined already) and mappings expressed as pairs, where first elements should
  be keywords indicating token rules (e.g. :drop or :keep) and second keywords
  indicating token classes.
  
  Token class may be expressed as a single keyword (if there is a single token
  class to assign) or as a sequential collection (vector, list and so on).
  
  Example:
  
  (defrules *config*
    :keep :letter
    :drop [:format :control]
  
  Returns a dynamic Var bound to token-classes map (character->:token-class)."
  [name & kvs]
  (let [dyname (with-meta name {:dynamic true})
        calls  (pmap #(cons 'assoc-rule %1) (partition-all 2 kvs))]
    `(let [v# (defonce-var ~dyname conf-seed)]
       (alter-var-root v# #(-> %1 (clear-rules) ~@calls))
       v#)))

(defn get-rule
  "Returns a token rule for a character given as the first argument."
  ([^Character chr & [mtok]]
   (let [tokrul (get-conf-section :rules   mtok)
         tokcls (get-conf-section :classes mtok)]
     (tokrul (tokcls chr) (:default tokrul)))))

;; Token classes generation.
;;

(defn classify-characters
  "Associates token classes with characters by creatnig mappings
  character-class-number->:token-class for a sequence of unicode characters
  given as the second argument and an existing confguration map as the first.
  
  The last argument should be a map containing
  pairs :character-class->:token-class, where the first element should be
  a keyword indicating character class and the second a keyword indicating
  a token class.

  Character classes are unicode character classes and token classes indicate
  some sets of characters that should be grouped and treated differently during
  tokenization. You can also pass key-value pairs as the rest of arguments
  instead of a map.
  
  Returns an updated map of tokenizer configuration."
  ([mtok class-mappings]
   (assoc-in mtok [:classes] (clojure.set/rename-keys
                              class-mappings type-to-unicode-class)))
  ([mtok fk & kvs]
   (classify-characters mtok (apply hash-map (cons fk kvs)))))

(defmacro genclasses
  "Creates a dynamic variable with name given as the first argument and binds it
  to a map containing mappings, where each unicode character is assigned to some
  token class. Takes key-value pairs of keywords (:character-type->:token-class)
  in order to create associations.
  
  Returns a dynamic Var bound to token-classes map (character->:token-class)."
  [name & kvs]
  (let [dyname (with-meta name {:dynamic true})]
    `(let [v# (defonce-var ~dyname conf-seed)]
       (alter-var-root
        v# #(-> %1
                (clear-classes)
                (classify-characters ~@kvs)))
       v#)))

;; Options management.

;; TODO

;; Tokenization.
;;

(defn get-char-info
  "Gets token class and token rule for a given character. Returns a map with
  keys :token-rule and :token-class."
  ([^Character chr & [mtok]]
   (let [r (get-rule  chr mtok)
         c (get-class chr mtok)]
     {:token-rule r :token-class c})))

(defn- tokenize-core
  [charseq classes rules opts]
  (lazy-seq
   (when-first [first-char charseq]
     (let [next-chars  (next charseq)
           token-class (classes (get-char-type-nr first-char))
           token-rule  (rules token-class (:default rules))
           part-chars  (take-while #(= token-class (classes (get-char-type-nr %1))) next-chars)
           results     (cons first-char part-chars)
           rest-chars  (drop (count part-chars) next-chars)
           next-call   (tokenize-core rest-chars classes rules opts)]
       (case token-rule
         :drop next-call
         :keep (cons (with-meta results {:token-class token-class}) next-call)
         (cons (with-meta results {:token-class token-class}) next-call))))))

(defn tokenize
  "Tokenizes a string or a sequence of characters."
  [text & {:as options}]
  (let [conf (:config options)
        opts (merge options (get-conf-section :options conf))
        ruls (get-conf-section :rules   conf)
        clss (get-conf-section :classes conf)
        chrs (ensure-char-seq text)]
    (tokenize-core chrs clss ruls opts)))

(defn tokenize-sentences
  "Tokenizes sequences of strings or sequence of character sequences."
  [texts & {:as options}]
  (let [conf (:config options)
        opts (merge options (get-conf-section :options conf) {:sentences true})
        ruls (get-conf-section :rules   conf)
        clss (get-conf-section :classes conf)
        seqs (ensure-sequential texts)
        tcal #(tokenize-core %1 clss ruls opts)]
    (map tcal seqs)))

(defn initialize
  "Initializes character->:token-class and :token-class->:token-rule mappings
  with default values."
  [& [option]]
  (if (= option :fast)
    nil
    ;;(load-classes *config* "resources/token-classes.clj")
    (genclasses *config*
                :UPPERCASE_LETTER          :letter
                :COMBINING_SPACING_MARK    :space
                :CONNECTOR_PUNCTUATION     :letter
                :CONTROL                   :control
                :CURRENCY_SYMBOL           :symbol
                :DASH_PUNCTUATION          :letter
                :DECIMAL_DIGIT_NUMBER      :number
                :ENCLOSING_MARK            :punctuation
                :END_PUNCTUATION           :punctuation
                :FINAL_QUOTE_PUNCTUATION   :punctuation
                :FORMAT                    :format
                :INITIAL_QUOTE_PUNCTUATION :punctuation
                :LETTER_NUMBER             :number
                :LINE_SEPARATOR            :separator
                :LOWERCASE_LETTER          :letter
                :MATH_SYMBOL               :letter
                :MODIFIER_LETTER           :letter
                :MODIFIER_SYMBOL           :symbol
                :NON_SPACING_MARK          :letter
                :OTHER_LETTER              :letter
                :OTHER_NUMBER              :number
                :OTHER_PUNCTUATION         :punctuation
                :OTHER_SYMBOL              :symbol
                :PARAGRAPH_SEPARATOR       :separator
                :PRIVATE_USE               :private
                :SPACE_SEPARATOR           :separator
                :START_PUNCTUATION         :punctuation
                :SURROGATE                 :letter
                :TITLECASE_LETTER          :letter
                :UNASSIGNED                :unassigned))
  
  (defrules *config*
    :default :keep
    :keep    [:letter :number :space :symbol :separator :punctuation]
    :drop    [:control :format :private]))

(initialize)

