(ns et.vp.ds.search.helpers
  (:require [clojure.string :as str]))

(defn remove-some-chars [q]
  (-> q
      (str/replace #"[\[\]()|!&':{}]+" " ")
      (str/replace "  " " ")
      (str/trim)))

(defn convert-q-to-query-string [q]
  (let [qs
        (str/join " & " (map #(str % ":*") (str/split (remove-some-chars q) #" ")))]
    (if (= ":*" qs)
      "*"
      qs)))

(defn get-search-clause [q]
  (when-not (= "" (or q ""))
    [:raw (format "searchable @@ to_tsquery('simple', '%s')"
                  (convert-q-to-query-string q))]))
