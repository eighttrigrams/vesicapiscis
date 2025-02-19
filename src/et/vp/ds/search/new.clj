(ns et.vp.ds.search.new
  (:require [et.vp.ds.search.core :as search.core]
            [et.vp.ds.search.helpers :as search.helpers]
            [honey.sql :as sql]))

(defn- get-search-clause [q]
  (when (not= "" q)
    [:raw (format "searchable @@ to_tsquery('simple', '%s')" 
                  (search.helpers/convert-q-to-query-string q))]))

(defn- get-events-exist-clause [events-view]
  (when (not= 0 events-view)
    [:and
     [:<> :issues.date nil]
     [:not= :issues.archived [:inline (= 1 events-view)]]]))

(defn- wrap-given-issues-query-with-limit [query {:keys [selected-context
                                                         join-ids
                                                         link-issue
                                                         search-mode
                                                         events-view
                                                         q]}]
  (prn "query" query)
  (merge 
   {:select (if selected-context 
              (vec (concat search.core/select [:collections.annotation]))
              search.core/select)
    :from   :issues
    :where  [:and [:in :issues.id query]
                  (if join-ids 
                    [:= :collections.container_id [:raw (:id selected-context)]]
                    true)]}
   {:order-by [[:issues.updated_at (if (and selected-context link-issue) 
                                     :desc
                                     (if (= 1 search-mode)
                                       :asc 
                                       :desc))]]}
   (when (or (and (= "" q)
                  (not selected-context)
                  (= 0 events-view))
             (and selected-context link-issue))
     {:limit 500})
   (when join-ids
     {:join [:collections [:= :issues.id :collections.item_id]]})))

(comment
  (sql/format (wrap-given-issues-query-with-limit {:select :ids 
                                                   :from   :issues
                                                   :where [:in [:issues.id [10 20]]]} 
                                                  {})))

(defn do-fetch-ids'' ;; TODO rename, it doesn't return only the ids
  [{:keys [q link-issue]
    :or   {q ""}} 
   selected-context
   search-mode
   events-view
   issue-ids-to-remove
   join-ids
   and-query?]
  #_(prn "and-query?" and-query? (some? selected-context) join-ids)
  (->
   (merge
    {:select :issues.id
     :from   [:issues]
     :where  [:and [:and
                    (get-events-exist-clause events-view)
                    (when join-ids [:in :collections.container_id [:inline join-ids]])
                    (get-search-clause q)]
              (when issue-ids-to-remove
                [:not [:in :issues.id [:inline issue-ids-to-remove]]])]}
    (when and-query?
      {:join     [:collections [:= :issues.id :collections.item_id]]
       :group-by :issues.id
       :having   [:raw (str "COUNT(issues.id) = " (count join-ids))]}))
   (wrap-given-issues-query-with-limit {:selected-context selected-context
                                        :join-ids         join-ids
                                        :link-issue       link-issue
                                        :search-mode      search-mode
                                        :events-view      events-view
                                        :q                q})
   (sql/format)
   #_(#(do (prn "#q" %) %))))
