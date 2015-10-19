(ns eneltron.chars
  ^{:doc "Eneltron library – character types and operations."
    :author "Paweł Wilk"}
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

(def all-chars
  "Lazy sequence of all characters. Note that some supplementary characters are
  represented as java.lang.String objects!"
  (char-range (Character/MIN_CODE_POINT)
              (Character/MAX_CODE_POINT)))

(defmacro ensure-char-seq
  "Ensures that the given argument is a sequence of characters.
  Returns a sequence."
  [s]
  `(let [s# ~s]
     (if (seq? s#) s# (char-seq s#))))

(defn get-char-type
  "Returns a type of a character given as the first argument."
  [^Character c] ; just Character?
  (unicode-class-to-type
   (try (Character/getType c)
        (catch ClassCastException e
          (Character/getType (code-point-of c)))) :UNASSIGNED))
