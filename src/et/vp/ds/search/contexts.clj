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

(defn- get-exclusion-clause [{:keys [link-context selected-context]}]
  (when 
    link-context
    [:not [:in :issues.id
           {:select :collections.container_id
            :from   :collections
            :where  [:= :collections.item_id (:id selected-context)]}]]))

(defn fetch-contexts
  [q {:keys [selected-context] :as opts}]
  (let [{:keys [limit]} opts
        exclusion-clause (get-exclusion-clause opts)]
    (sql/format 
     (merge {:select   contexts-select
             :from     [:issues]
             :where    [:and
                        (get-search-clause q)
                        [:= :issues.is_context true]
                        [:not [:= :issues.id (:id selected-context)]]
                        exclusion-clause]
             :order-by [[:updated_at_ctx :desc]]}
            (when true {:limit (or limit 100)})))))