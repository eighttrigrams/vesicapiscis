(ns et.vp.ds.search.items
  (:require [et.vp.ds.search.core :as core]
            [et.vp.ds.search.helpers :as search.helpers]
            [honey.sql :as sql]))

(defn exclusion-clause [selected-context-id mode]
  [:not [:in :issues.id 
         (if (= :issues mode)
           {:select :collections.item_id
            :from   :collections
            :where  [:= :collections.container_id [:inline selected-context-id]]}
           {:select :collections.container_id
            :from   :collections
            :where  [:= :collections.item_id selected-context-id]})]])

(defn fetch-items
  [q {:keys [selected-context all-items? link-context link-issue] :as opts}]
  (let [{:keys [limit]} opts
        exclusion-clause (when (or link-context link-issue)
                           (exclusion-clause (:id (:selected-context opts))
                                             (if link-issue
                                               :issues
                                               :contexts)))]
    (sql/format  
     {:select   core/select
      :from     [:issues]
      :where    [:and
                 (search.helpers/get-search-clause q)
                 (when-not all-items? [:= :issues.is_context true])
                 (when selected-context [:not [:= :issues.id (:id selected-context)]])
                 (when selected-context exclusion-clause)]
      :order-by [[(if all-items? 
                   :issues.updated_at
                   :issues.updated_at_ctx) :desc]]
      :limit (or limit 100)})))