(ns eneltron.tokens
  ^{:doc "Eneltron library – tokenization support."
    :author "Paweł Wilk"}
  (:require [eneltron.chars :refer :all]))

;; Character-to-token-class and token-class-to-operation mappings basic
;; structure.

(def tokens-seed {:__ops {}})

;; Default token mappings.

(def ^:dynamic *tokens* tokens-seed)

;; Character operations.
;;

(defn get-char-token-class
  "Returns a token class (keyword) of a character given as the last argument. If
  there are two arguments, the first of them should be a map describing token
  classes and rules. Otherwise *tokens* special variable will be used as
  the source of mappings."
  ([^Character c]        (*tokens* (get-char-type c)))
  ([tokens ^Character c] (tokens   (get-char-type c))))

;; Token classes operations.

(defn assoc-char
  "Associates character given as the last argument with a token class given as
  the second argument by updating a map passed as the first argument.
  
  Returns an updated map."
  [mtok token-class ^Character c]
  (assoc mtok c token-class))

(defn assoc-chars
  "Associates each character from a sequence of characters given as a last
  argument with a token class given as a third argument by updating a map passed
  as a first argument.
  
  Returns an updated map."
  [mtok token-class chars]
  (reduce #(assoc %1 %2 token-class) mtok (ensure-char-seq chars)))

(defmacro assoc-chars->
  "Updates a map given as the first argument in a way that it associates each
  character from a sequence of characters (or a string) given as the last
  elements of pairs with token classes given as the first elements of pairs.
  with a token class given as a third argument by updating a map passed as
  a first argument.
  
  Returns an updated map."
  [mtok & kvs]
  (let [calls (map #(cons 'assoc-chars %1) (partition-all 2 kvs))]
    `(-> ~mtok ~@calls)))

(defmacro deftokens
  "Creates mappings character->:token-class. Takes name of dynamic variable to
  define and mappings expressed as pairs, where first element should be
  a character and second a keyword naming its class."
  [name & kvs]
  (let [dyname (with-meta name {:dynamic true})
        calls  (map #(cons 'assoc-chars %1) (partition-all 2 kvs))]
    `(def ~dyname (-> tokens-seed ~@calls))))

(defn get-token-class
  "Gets token class for a given character."
  ([^Character chr]        (*tokens* chr)) 
  ([^Character chr tokens] (tokens   chr)))

;; Token rules operations.
;;

(defn clear-rules
  "Takes token mappings and clears rules section. Returns new map with rules
  cleared."
  [mtok]
  (assoc mtok :__ops {}))

(defn- reduce-ops
  "Helper function that updates operations branch in tokens map using reduce on
  token classes."
  [f mtok token-classes]
  (update-in mtok [:__ops] #(reduce f %1 token-classes)))

(defn assoc-rule
  "Associates a token rule given as the second argument with a token class given
  as the last argument by updating a map passed as the first argument. If there
  are more arguments, it does that for any element (any token class).
  
  Returns an updated map."
  ([mtok token-rule token-classes]
   (let [tclasses (if (seq? token-classes) token-classes (list token-classes))]
     (reduce-ops #(assoc %1 %2 token-rule) mtok tclasses)))
  ([mtok token-rule token-class & other-classes]
   (assoc-rule mtok token-rule (cons token-class other-classes))))

(defmacro defrules
  [name & kvs]
  "Definies new set of rules for token classes by creatnig mappings
  :token-class->:token-rule. Takes name of dynamic variable to define (if not
  defined already) and mappings expressed as pairs, where first elements should
  be keywords indicating token rules (e.g. :drop or :keep) and second keywords
  indicating token classes.

  Token class may be expressed as a single keyword (if there is a single token
  class to assign) or as a sequential collection (vector, list and so on).

  Example:

  (defrules *tokens*
    :keep :letter
    :drop [:format :control]"
  (let [dyname (with-meta name {:dynamic true})
        calls  (map #(cons 'assoc-rule %1) (partition-all 2 kvs))]
    `(do
       (defonce ~dyname tokens-seed)
       (alter-var-root (var ~dyname) #(-> %1 (clear-rules) ~@calls)))))

(defn get-token-rule
  "Returns a token rule for a character given as the first argument."
  ([^Character chr] (get-token-rule chr *tokens*))
  ([^Character chr tokens]
   (let [tokops (:__ops tokens)]
     (tokops (tokens chr)))))

;; Tokens generation.
;;

(defn classify-characters
  "Associates token classes with characters by creatnig mappings
  character->:token-class based upon a sequence of unicode characters given as
  the first argument and existing map as a second.
  
  The last argument should be a map containing
  pairs :character-class->:token-class, where the first element should be
  a keyword indicating character class and the second a keyword indicating
  a token class. Character classes are unicode character classes, token classes
  are groups of characters that should be treated differently during
  tokenization. You can also pass key-value pairs as the rest of arguments
  instead of a map.
  
  Returns an updated map of classified characters."
  ([chars mtok class-mappings]
   (if-let [first-char (first chars)]
     (let [next-chars  (next chars)
           tokenclass  (class-mappings (get-char-type first-char))
           part-chars  (take-while
                        #(= tokenclass (class-mappings (get-char-type %1)))
                        next-chars)
           characters  (cons first-char part-chars)
           rest-chars  (drop (count part-chars) next-chars)
           new-tokens  (reduce #(assoc %1 %2 tokenclass) mtok characters)]
       (recur rest-chars new-tokens class-mappings))
     mtok))
  ([chars mtok fk & kvs]
   (classify-characters chars mtok (apply hash-map (cons fk kvs)))))

(defmacro gentokens
  "Creates a dynamic variable with name given as the first argument and binds it
  to a map containing mappings, where each unicode character is assigned to some
  token class. Takes key-value pairs of keywords (:character-type->:token-class)
  in order to create associations.
  
  Returns a token-classes map (character->:token-class)."
  [name & kvs]
  (let [dyname (with-meta name {:dynamic true})]
    `(def ~dyname (classify-characters all-chars tokens-seed ~@kvs))))

;; Tokenization.
;;

(defn tokenize
  ([chars] (tokenize chars *tokens*))
  ([chars tokens] (tokenize (ensure-char-seq chars) tokens (:__ops tokens)))
  ([chars tokens token-rules]
   (lazy-seq
    (when-first [first-char chars]
      (let [next-chars  (next chars)
            token-class (tokens first-char)
            token-rule  (token-rules token-class)
            part-chars  (take-while #(= token-class (tokens %1)) next-chars)
            results     (cons first-char part-chars)
            rest-chars  (drop (count part-chars) next-chars)
            exe         (tokenize rest-chars tokens token-rules)]
        (if (= :drop token-rule) exe (cons results exe)))))))

(gentokens *tokens*
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
           :UNASSIGNED                :unassigned)

(def tokrule-to-tokop
  {:keep [:letter :number :space :symbol :separator :punctuation]
   :drop [:control :format :private]})


;;(tokenize "Siała baba mak. Nie wiedziała jak. Raz, dwa, trzy – oraz jeszcze –
;;cztery.")

