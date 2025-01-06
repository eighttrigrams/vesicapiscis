(ns et.vp.ds.search.new
  (:require [et.vp.ds.search.core :as search.core]
            [et.vp.ds.search.helpers :as search.helpers]
            [honey.sql :as sql]))

(defn- wrap-order-and-limit [formatted-query selected-context link-issue]
  (let [formatted-query (if (and selected-context link-issue) 
                          (let [[q :as original-query] formatted-query
                                formatted-query        (str "SELECT * FROM (" q ") AS issues ORDER BY issues.updated_at DESC LIMIT 500")]
                            (assoc original-query 0 formatted-query))
                          formatted-query)]
    formatted-query))

(defn- get-search-clause [q]
  (when (not= "" q)
    [:raw (format "searchable @@ to_tsquery('simple', '%s')" 
                  (search.helpers/convert-q-to-query-string q))]))

(defn- get-events-exist-clause [events-view]
  (when (not= 0 events-view)
    [:and
     [:<> :issues.date nil]
     [:not= :issues.archived [:inline (= 1 events-view)]]]))

(defn do-fetch-ids'' 
  [{:keys [q link-issue]
    :or   {q ""}} 
   selected-context
   search-mode
   events-view
   issue-ids-to-remove
   join-ids
   and-query?]
  (-> 
   (sql/format 
    (merge
     {:select (if selected-context (vec (concat search.core/select [:collections.annotation]))
                  search.core/select)
      :from   [:issues]
      :where  [:and [:and
                     (get-events-exist-clause events-view)
                     (when join-ids [:in :collections.container_id [:inline join-ids]])
                     (get-search-clause q)]
               (when issue-ids-to-remove
                 [:not [:in :issues.id [:inline issue-ids-to-remove]]])]}
     (when join-ids
       {:group-by (if selected-context
                    [:issues.id :collections.annotation]
                    [:issues.id])
        :join     [:collections [:= :issues.id :collections.item_id]]})
     (when and-query?
       {:having [:raw (str "COUNT(issues.id) = " (count join-ids))]})
     (when-not (and selected-context link-issue)
       {:order-by [[:issues.updated_at (if (= 1 search-mode)
                                         :asc 
                                         :desc)]]})
     (when (and (= "" q)
                (not selected-context)
                (= 0 events-view))
       {:limit 500})))
    ;; TODO i could do the sorting and limiting uniformly here
   (wrap-order-and-limit selected-context link-issue)))
