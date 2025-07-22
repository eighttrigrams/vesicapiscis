(ns et.vp.ds.search.contexts
  (:require [et.vp.ds.search.helpers :as search.helpers]
            [honey.sql :as sql]))

(def contexts-select 
  [:issues.title
   :issues.short_title
   :issues.sort_idx
   :issues.id
   :issues.data
   :issues.is_context
   :issues.updated_at_ctx])

(defn- get-search-clause [q]
  (when-not (= "" (or q ""))
    [:raw (format "searchable @@ to_tsquery('simple', '%s')"
                  (search.helpers/convert-q-to-query-string q))]))

(defn- parse-context-id [id]
  (if (number? id)
    id
    (Integer/parseInt (if (keyword? id) (name id) id))))

(defn filter-contexts [{:keys [link-context selected-context selected-issue]} contexts]
  (if-not link-context
    (remove #(= (:id selected-context) (:id %)) contexts)
    (let [context-keys (keys (or (:contexts (:data selected-issue))
                                 (:contexts (:data selected-context))))
          ids-of-contexts-to-remove (conj (set (map parse-context-id context-keys))
                                          (:id (or selected-issue selected-context)))]
      (remove #(ids-of-contexts-to-remove (:id %)) contexts))))

(defn fetch-contexts
  [q {:keys [limit]}]
  (sql/format 
   (merge {:select   contexts-select
           :from     [:issues]
           :where    [:and
                      (get-search-clause q)
                      [:= :issues.is_context true]]
           :order-by [[:updated_at_ctx :desc]]}
          (when true {:limit (or limit 100)}))))