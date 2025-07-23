(ns et.vp.ds.search.related-items
  (:require [cambium.core :as log]
            [et.vp.ds.search.core :as core]
            [et.vp.ds.search.helpers :as search.helpers]
            [honey.sql :as sql]))

(defn- get-events-exist-clause [search-mode]
  (when (= 4 search-mode)
    [:<> :issues.date nil]))

(defn- and-query 
  [join-ids unassigned-mode? inverted-mode?]
  (let [r
        [:in :issues.id
         (merge {:select   :issues.id
                 :from     [:issues]
                 :join     [:collections [:= :issues.id :collections.item_id]]
                 :group-by :issues.id
                 :having   [:raw (str "COUNT(issues.id) = " 
                                      (if unassigned-mode? 1
                                          (count join-ids)))]}
                (when-not unassigned-mode? {:where [:in :collections.container_id [:inline join-ids]]}))]]
    (if inverted-mode?
      [:not r]
      r)))

(defn- or-partial [join-ids]
  {:select :issues.id
   :from   [:issues]
   :join   [:collections [:= :issues.id :collections.item_id]]
   :where  [:in :collections.container_id [:inline join-ids]]})

(defn- or-query 
  [join-ids unassigned-mode?]
  (if unassigned-mode?
    [:and
     [:not [:in :issues.id
            (or-partial join-ids)]]
     [:in :issues.id
      {:select   :issues.id
       :from     [:issues]
       :join     [:collections [:= :issues.id :collections.item_id]]
       :group-by :issues.id
       :having   [:raw "COUNT(issues.id) > 1"]}]]
    [:not [:in :issues.id
           (or-partial join-ids)]]))

(defn- order-by [search-mode]
  [(if (= search-mode 5)
     [:issues.inserted_at :desc]
     (if (= search-mode 4)
       [:issues.date :desc]
       (if (or (= 2 search-mode) (= 3 search-mode))
         [:issues.sort_idx (if (= 2 search-mode)
                                     :asc
                                     :desc)]
         [:issues.updated_at (if (= 1 search-mode)  
                               :asc
                               :desc)])))])

(defn- limit' [{:keys [selected-context-id exclude-id?]}
               {:keys [force-limit? limit]}]
  (when (or (not selected-context-id)
            ;; TODO not sure if that param is still needed
            force-limit?
            exclude-id?)
     {:limit limit}))

(defn- wrap-given-issues-query-with-limit
  "if exclude-id? is set in opts
   - will ignore join ids
   - will limit the results"
  [q 
   {:keys [selected-context-id
           join-ids
           search-mode
           unassigned-mode?
           inverted-mode?
           exclude-id?]
    :as   opts}
   {:keys [limit]
    :as ctx}]
  (when-not limit (throw (IllegalArgumentException. "no limit provided")))
  (let [join-ids (when-not exclude-id? join-ids)
        exclude-id (when exclude-id? selected-context-id)
        selected-context-id (when-not exclude-id? selected-context-id)
        join-ids (when selected-context-id join-ids)
        or-mode? (when join-ids inverted-mode?)]
    (merge
     {:select (if
               ;; TODO get rid of if; use items search instead of else branch
               selected-context-id
                (vec (concat core/select [:collections.annotation]))
                core/select)
      :from   :issues
      :where  [:and
               (when (or join-ids unassigned-mode?)
                 (if or-mode? 
                   (or-query join-ids unassigned-mode?) 
                   (and-query join-ids unassigned-mode? inverted-mode?)))
               (search.helpers/get-search-clause q)
               (get-events-exist-clause search-mode)
               ;; TODO get rid of when; use items search instead
               (when selected-context-id
                 [:= :collections.container_id [:raw selected-context-id]])
               (when (or (= 2 search-mode) (= 3 search-mode))
                 [:> :sort_idx 0])
               (when exclude-id
                 (core/exclusion-clause exclude-id :issues))
               (when exclude-id
                  [:<> :issues.id [:inline exclude-id]])]}
     {:order-by (order-by search-mode)}
     (limit' opts ctx)
     ;; TODO get rid of when; use items search instead
     (when selected-context-id
       {:join [:collections [:= :issues.id :collections.item_id]]}))))

(defn fetch-items
  [q 
   opts
   ctx]
  (->
   (wrap-given-issues-query-with-limit q opts ctx)
   (sql/format)))
