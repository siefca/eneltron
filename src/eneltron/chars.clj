(ns ^{:doc "Eneltron library, character types and operations."
      :author "Paweł Wilk"}
    
    eneltron.chars
  
  (:require [djy.char :as ch]
            [djy.char :exclude [next symbol?] :refer :all]))

;; Unicode character classess defined as numeric constants
;; mapped to keywords.

(def unicode-class-to-type
  {Character/UPPERCASE_LETTER          :UPPERCASE_LETTER
   Character/COMBINING_SPACING_MARK    :COMBINING_SPACING_MARK
   Character/CONNECTOR_PUNCTUATION     :CONNECTOR_PUNCTUATION
   Character/CONTROL                   :CONTROL
   Character/CURRENCY_SYMBOL           :CURRENCY_SYMBOL
   Character/DASH_PUNCTUATION          :DASH_PUNCTUATION
   Character/DECIMAL_DIGIT_NUMBER      :DECIMAL_DIGIT_NUMBER
   Character/ENCLOSING_MARK            :ENCLOSING_MARK
   Character/END_PUNCTUATION           :END_PUNCTUATION
   Character/FINAL_QUOTE_PUNCTUATION   :FINAL_QUOTE_PUNCTUATION
   Character/FORMAT                    :FORMAT
   Character/INITIAL_QUOTE_PUNCTUATION :INITIAL_QUOTE_PUNCTUATION
   Character/LETTER_NUMBER             :LETTER_NUMBER
   Character/LINE_SEPARATOR            :LINE_SEPARATOR
   Character/LOWERCASE_LETTER          :LOWERCASE_LETTER
   Character/MATH_SYMBOL               :MATH_SYMBOL
   Character/MODIFIER_LETTER           :MODIFIER_LETTER
   Character/MODIFIER_SYMBOL           :MODIFIER_SYMBOL
   Character/NON_SPACING_MARK          :NON_SPACING_MARK
   Character/OTHER_LETTER              :OTHER_LETTER
   Character/OTHER_NUMBER              :OTHER_NUMBER
   Character/OTHER_PUNCTUATION         :OTHER_PUNCTUATION
   Character/OTHER_SYMBOL              :OTHER_SYMBOL
   Character/PARAGRAPH_SEPARATOR       :PARAGRAPH_SEPARATOR
   Character/PRIVATE_USE               :PRIVATE_USE
   Character/SPACE_SEPARATOR           :SPACE_SEPARATOR
   Character/START_PUNCTUATION         :START_PUNCTUATION
   Character/SURROGATE                 :SURROGATE
   Character/TITLECASE_LETTER          :TITLECASE_LETTER
   Character/UNASSIGNED                :UNASSIGNED})

(def type-to-unicode-class
  (clojure.set/map-invert unicode-class-to-type))

;; Special characters.
;;

(def brackets
  {\(  \) \[  \]  \u2045 \u2046  \< \>  \{ \}
   \u2308 \u2309  \u230A \u230B  \u239B \u239E
   \u239C \u239F  \u239D \u23A0  \u23A1 \u23A4
   \u23A2 \u23A5  \u23A3 \u23A6  \u23A7 \u23AB
   \u23A8 \u23AC  \u23A9 \u23AD  \u23AA nil
   \u23B0 \u23B1  \u23B4 \u23B5  \u23B6 nil
   \u23B8 \u23B9  \u23DC \u23DD  \u23DE \u23DF
   \u23E0 \u23E1  \u27E6 \u27E7  \u27E8 \u27E9
   \u27EA \u27EB  \u27EC \u27ED  \u27EE \u27EF
   \u2983 \u2984  \u2985 \u2986  \u2987 \u2988
   \u2989 \u298A  \u298B \u298C  \u298D \u298E
   \u298F \u2990  \u2991 \u2992  \u2993 \u2994
   \u2995 \u2996  \u2997 \u2998  \u2E22 \u2E23
   \u2E24 \u2E25  \uFF08 \uFF09  \uFF3B \uFF3D
   \uFF1C \uFF1E  \uFF5B \uFF5D})

(def quotation-marks
  {\«     \»      \"     \"      \'     \'
   \u201C \u201D  \u2018 \u2019  \u201A \u201A
   \u201B \u201B  \u201C \u201D  \u201E nil
   \u201F nil     \u2039 \u203A  \u231C \u231D
   \u231E \u231F  \u2329 \u232A  \uFF62 \uFF63
   \u3008 \u3009  \u300A \u300B  \u300C \u300D
   \u300E \u300F  \u3010 \u3011  \uFE41 \uFE42
   \uFE43 \uFE44  \uFF02 \uFF02  \uFF07 \uFF07
   \uFF62 \uFF63})

(def all-chars
  "Lazy sequence of all characters. Note that some supplementary characters are
  represented as java.lang.String objects!"
  (char-range (Character/MIN_CODE_POINT)
              (Character/MAX_CODE_POINT)))

;; Character operations.
;;

(defmacro ensure-char-seq
  "Ensures that the given argument is a sequence of characters.
  Returns a sequence."
  [s]
  `(let [s# ~s]
     (if (seq? s#) s# (char-seq s#))))

(defn get-char-type-nr
  "Returns a type number of a character given as the first argument."
  [^Character c]
  (try (Character/getType c)
       (catch ClassCastException e
         (Character/getType (code-point-of c)))))

(defn get-char-type
  "Returns a type of a character given as the first argument."
  [^Character c]
  (unicode-class-to-type (get-char-type-nr c) :UNASSIGNED))

