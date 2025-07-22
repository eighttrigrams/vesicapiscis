(ns et.vp.ds.search.contexts
  (:require [et.vp.ds.search.core :as core]
            [et.vp.ds.search.helpers :as search.helpers]
            [honey.sql :as sql]))

(defn- get-exclusion-clause [{:keys [link-context selected-context]}]
  (when 
    link-context
    [:not [:in :issues.id
           {:select :collections.container_id
            :from   :collections
            :where  [:= :collections.item_id (:id selected-context)]}]]))

;; TODO make it work for items or for contexts
(defn fetch-items
  [q {:keys [selected-context] :as opts}]
  (let [{:keys [limit]} opts
        exclusion-clause (get-exclusion-clause opts)]
    (sql/format 
     (merge {:select   core/select
             :from     [:issues]
             :where    [:and
                        (search.helpers/get-search-clause q)
                        [:= :issues.is_context true]
                        [:not [:= :issues.id (:id selected-context)]]
                        exclusion-clause]
             :order-by [[:updated_at_ctx :desc]]}
            (when true {:limit (or limit 100)})))))