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

;; TODO when we specify link-issue, we shouldn't have to also pass all-items? since this is implied; same with link-context
(defn search
  [q 
   {:keys [selected-context-id all-items? link-context link-issue] :as _opts}
   {:keys [limit] :as _ctx}]
  (let [exclusion-clause (when (or link-context link-issue)
                           (exclusion-clause selected-context-id
                                             (if link-issue
                                               :issues
                                               :contexts)))]
    (sql/format  
     (merge {:select   core/select
             :from     [:issues]
             :where    [:and
                        (search.helpers/get-search-clause q)
                        (when-not all-items? [:= :issues.is_context true])
                        (when selected-context-id [:not [:= :issues.id selected-context-id]])
                        (when selected-context-id exclusion-clause)]
             :order-by [[(if all-items? 
                           :issues.updated_at
                           :issues.updated_at_ctx) :desc]]}
            (when limit {:limit limit})))))