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

(defn- and-query 
  [{:keys [join-ids]}]
  [:in :issues.id
   (merge
    {:select   :issues.id
     :from     [:issues]
     :where    [:and (when
                   ;; TODO review necessity of when condition
                      join-ids [:in :collections.container_id [:inline join-ids]])]
     :join     [:collections [:= :issues.id :collections.item_id]]
     :group-by :issues.id
     :having   [:raw (str "COUNT(issues.id) = " (count join-ids))]})])

(defn- order-by [events-view search-mode]
  [(if (= 0 events-view)
                 (if (or (= 2 search-mode) (= 3 search-mode ))
                   [:issues.short_title_ints (if (= 2 search-mode)
                                               :asc
                                               :desc)]
                   [:issues.updated_at (if (= 1 search-mode)  
                                         :asc
                                         :desc)])
                 [:issues.date (if (= 1 events-view)
                                 :asc
                                 :desc)])])

(defn- limit [{:keys [selected-context
                      link-issue
                      events-view
                      q]}]
  (when (or (and (= "" q)
                  (not selected-context)
                  (= 0 events-view))
             link-issue)
     {:limit 500}))

(defn- wrap-given-issues-query-with-limit
  [issue-ids-to-remove
   {:keys [selected-context
           join-ids
           search-mode
           events-view
           and-query?
           q]
    :as opts}]
  #_(prn "query" query)
  (merge 
   {:select (if selected-context 
              (vec (concat search.core/select [:collections.annotation]))
              search.core/select)
    :from   :issues
    :where  [:and
             (when and-query? (and-query opts))
             (when issue-ids-to-remove
                [:not [:in :issues.id [:inline issue-ids-to-remove]]])
             (get-search-clause q)
             (get-events-exist-clause events-view)
             (if join-ids 
               [:= :collections.container_id [:raw (:id selected-context)]]
               ;; TODO get rid of true and use when instead if
               true)
             (when (or (= 2 search-mode) (= 3 search-mode))
               [:> :short_title_ints 0])]}
   {:order-by (order-by events-view search-mode)}
   (limit opts)
   (when join-ids
     {:join [:collections [:= :issues.id :collections.item_id]]})))

(comment
  #_(sql/format (wrap-given-issues-query-with-limit {:select :ids 
                                                   :from   :issues
                                                   :where [:in [:issues.id [10 20]]]} 
                                                  {})))

(defn fetch-issues
  [{:keys [q link-issue]
    :or   {q ""}} 
   issue-ids-to-remove
   {:as opts}]
  #_(prn "and-query?" and-query? (some? selected-context) join-ids)
  (let [opts (assoc opts :q q :link-issue link-issue)]
    (->
     (wrap-given-issues-query-with-limit issue-ids-to-remove opts)
     (sql/format)
     #_(#(do (prn "#q" %) %)))))
