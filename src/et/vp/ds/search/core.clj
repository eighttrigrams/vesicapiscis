(ns et.vp.ds.search.core
  (:require [et.vp.ds.search.core :as core]
            [honey.sql :as sql]
            [clojure.string :as str]))

(def select [:issues.title
             :issues.short_title
             :issues.sort_idx
             :issues.id
             :issues.data
             ;; TODO make tags optional; i need it only in tracker-mcp
             :issues.tags
             [:issues.annotation :issue_annotation]
             :issues.is_context
             :issues.inserted_at
             :issues.updated_at
             :issues.date])

(defn exclusion-clause [selected-context-id mode]
  [:not [:in :issues.id 
         (if (= :issues mode)
           {:select :relations.item_id
            :from   :relations
            :where  [:= :relations.container_id [:inline selected-context-id]]}
           {:select :relations.container_id
            :from   :relations
            :where  [:= :relations.item_id selected-context-id]})]])

(defn- remove-some-chars [q]
  (-> q
      (str/replace #"[\[\]()|!&':{}]+" " ")
      (str/replace "  " " ")
      (str/trim)))

(defn convert-q-to-query-string [q]
  (let [qs
        (str/join " & " (map #(str % ":*") (str/split (remove-some-chars (or q "")) #" ")))]
    (if (= ":*" qs)
      "*"
      qs)))

(defn get-search-clause [q]
  (when-not (= "" (or q ""))
    [:raw (format "searchable @@ to_tsquery('simple', '%s')"
                  (convert-q-to-query-string q))]))

(defn search-items
  [q 
   {:keys [selected-context-id all-items? link-context link-issue] :as _opts}
   {:keys [limit] :as _ctx}]
  (let [exclusion-clause (when (or link-context link-issue)
                           (exclusion-clause selected-context-id
                                             (if link-issue
                                               :issues
                                               :contexts)))]
    (sql/format  
     (merge {:select   select
             :from     [:issues]
             :where    [:and
                        (get-search-clause q)
                        (when-not all-items? [:= :issues.is_context true])
                        (when selected-context-id [:not [:= :issues.id selected-context-id]])
                        (when selected-context-id exclusion-clause)]
             :order-by [[(if all-items? 
                           :issues.updated_at
                           :issues.updated_at_ctx) :desc]]}
            (when limit {:limit limit})))))

(defn- get-events-exist-clause [search-mode]
  (when (= 4 search-mode)
    [:<> :issues.date nil]))

(defn- and-query 
  [join-ids unassigned-mode? inverted-mode?]
  (let [r
        [:in :issues.id
         (merge {:select   :issues.id
                 :from     [:issues]
                 :join     [:relations [:= :issues.id :relations.item_id]]
                 :group-by :issues.id
                 :having   [:raw (str "COUNT(issues.id) = " 
                                      (if unassigned-mode? 1
                                          (count join-ids)))]}
                (when-not unassigned-mode? {:where [:in :relations.container_id [:inline join-ids]]}))]]
    (if inverted-mode?
      [:not r]
      r)))

(defn- or-partial [join-ids]
  {:select :issues.id
   :from   [:issues]
   :join   [:relations [:= :issues.id :relations.item_id]]
   :where  [:in :relations.container_id [:inline join-ids]]})

(defn- or-query 
  [join-ids unassigned-mode?]
  (if unassigned-mode?
    [:and
     [:not [:in :issues.id
            (or-partial join-ids)]]
     [:in :issues.id
      {:select   :issues.id
       :from     [:issues]
       :join     [:relations [:= :issues.id :relations.item_id]]
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

(defn search-related-items
  [q 
   {:keys [selected-context-id
           join-ids
           search-mode
           unassigned-mode?
           inverted-mode?]
    :as   _opts}
   {:keys [limit]
    :as _ctx}]
  (let [or-mode? (when join-ids inverted-mode?)]
    (-> (merge
         {:select (vec (concat core/select [:relations.annotation]))
          :from   :issues
          :where  [:and
                   (when (or join-ids unassigned-mode?)
                     (if or-mode? 
                       (or-query join-ids unassigned-mode?) 
                       (and-query join-ids unassigned-mode? inverted-mode?)))
                   (get-search-clause q)
                   (get-events-exist-clause search-mode)
                   [:= :relations.container_id [:raw selected-context-id]]
                   (when (or (= 2 search-mode) (= 3 search-mode))
                     [:> :sort_idx 0])]}
         {:order-by (order-by search-mode)}
         (when limit {:limit limit})
         {:join [:relations [:= :issues.id :relations.item_id]]})
        sql/format)))
