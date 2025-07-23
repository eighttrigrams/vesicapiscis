(ns et.vp.ds.search.items
  (:require [et.vp.ds.search.core :as core]
            [et.vp.ds.search.helpers :as search.helpers]
            [honey.sql :as sql]
            [cambium.core :as log]))

(defn- get-exclusion-clause [{:keys [link-context selected-context]}]
  (when 
    link-context
    [:not [:in :issues.id
           {:select :collections.container_id
            :from   :collections
            :where  [:= :collections.item_id (:id selected-context)]}]]))

(defn fetch-items
  [q {:keys [selected-context all-items?] :as opts}]
  (let [{:keys [limit]} opts
        exclusion-clause (get-exclusion-clause opts)]
    (log/info (str "fetch-items limit: " limit ". -> " (or limit 100)))
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