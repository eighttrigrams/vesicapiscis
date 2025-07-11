(ns et.vp.ds.search.new
  (:require [et.vp.ds.search.core :as search.core]
            [et.vp.ds.search.helpers :as search.helpers]
            [honey.sql :as sql]))

(defn- get-search-clause [q]
  (when (not= "" q)
    [:raw (format "searchable @@ to_tsquery('simple', '%s')" 
                  (search.helpers/convert-q-to-query-string q))]))

(defn- get-events-exist-clause [search-mode]
  (when (= 4 search-mode)
    [:<> :issues.date nil]))

(defn- and-query 
  [{:keys [join-ids]}]
  [:in :issues.id
   (merge
    {:select   :issues.id
     :from     [:issues]
     :where    [:and (when
                      join-ids [:in :collections.container_id [:inline join-ids]])]
     :join     [:collections [:= :issues.id :collections.item_id]]
     :group-by :issues.id
     :having   [:raw (str "COUNT(issues.id) = " (count join-ids))]})])

(defn- order-by [search-mode]
  [(if (= search-mode 5)
     [:issues.inserted_at :desc]
     (if (= search-mode 4)
       [:issues.date :desc]
       (if (or (= 2 search-mode) (= 3 search-mode))
         [:issues.short_title_ints (if (= 2 search-mode)
                                     :asc
                                     :desc)]
         [:issues.updated_at (if (= 1 search-mode)  
                               :asc
                               :desc)])))])

(defn- limit [q {:keys [selected-context
                      force-limit?]}]
  (when (or (and (= "" q)
                 (not selected-context))
             force-limit?)
     {:limit 500}))

(defn- wrap-given-issues-query-with-limit
  [q {:keys [selected-context
           join-ids
           search-mode
           and-query?]
    :as opts}]
  (merge 
   {:select (if selected-context 
              (vec (concat search.core/select [:collections.annotation]))
              search.core/select)
    :from   :issues
    :where  [:and
             (when and-query? (and-query opts))
             (get-search-clause q)
             (get-events-exist-clause search-mode)
             (when join-ids 
               [:= :collections.container_id [:raw (:id selected-context)]])
             (when (or (= 2 search-mode) (= 3 search-mode))
               [:> :short_title_ints 0])]}
   {:order-by (order-by search-mode)}
   (limit q opts)
   (when join-ids
     {:join [:collections [:= :issues.id :collections.item_id]]})))

(defn fetch-issues
  [q 
   {:as opts}]
  (->
   (wrap-given-issues-query-with-limit q opts)
   (sql/format)))
