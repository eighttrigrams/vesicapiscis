(ns et.vp.ds.search.core
  (:require [et.vp.ds.search.core :as core]
            [honey.sql :as sql]
            [clojure.string :as str]))

(def select
  [:items.title :items.short_title :items.sort_idx :items.id :items.data
   ;; TODO make tags optional; i need it only in tracker-mcp
   :items.tags [:items.annotation :item_annotation] :items.is_context :items.inserted_at
   :items.updated_at :items.date])

(defn exclusion-clause
  [selected-item-id mode]
  [:not
   [:in :items.id
    (if (= :items mode)
      {:select :relations.target_id
       :from :relations
       :where [:= :relations.owner_id [:inline selected-item-id]]}
      {:select :relations.owner_id
       :from :relations
       :where [:= :relations.target_id selected-item-id]})]])

(defn- remove-some-chars
  [q]
  (-> q
      (str/replace #"[\[\]()|!&':{}]+" " ")
      (str/replace "  " " ")
      (str/trim)))

(defn convert-q-to-query-string
  [q]
  (let [qs (str/join " & " (map #(str % ":*") (str/split (remove-some-chars (or q "")) #" ")))]
    (if (= ":*" qs) "*" qs)))

(defn get-search-clause
  [q]
  (when-not (= "" (or q ""))
    [:raw (format "searchable @@ to_tsquery('simple', '%s')" (convert-q-to-query-string q))]))

(defn search-items
  [q {:keys [selected-item-id all-items? link-context link-item] :as _opts}
   {:keys [limit] :as _ctx}]
  (let [exclusion-clause (when (or link-context link-item)
                           (exclusion-clause selected-item-id (if link-item :items :contexts)))]
    (sql/format (merge {:select select
                        :from [:items]
                        :where [:and (get-search-clause q)
                                (when-not all-items? [:= :items.is_context true])
                                (when selected-item-id [:not [:= :items.id selected-item-id]])
                                (when selected-item-id exclusion-clause)]
                        :order-by [[(if all-items? :items.updated_at :items.updated_at_ctx) :desc]]}
                       (when limit {:limit limit})))))

(defn- get-events-exist-clause [search-mode] (when (= 4 search-mode) [:<> :items.date nil]))

(defn- get-description-filter-clause
  [description-filter]
  (case description-filter
    (true :only "only") [:and [:<> :items.description nil] [:not [:= :items.description ""]]]
    (false :no "no") [:or [:= :items.description nil] [:= :items.description ""]]
    nil))

(defn- and-query
  [join-ids unassigned-mode? inverted-mode?]
  (let [r [:in :items.id
           (merge
             {:select :items.id
              :from [:items]
              :join [:relations [:= :items.id :relations.target_id]]
              :group-by :items.id
              :having [:raw (str "COUNT(items.id) = " (if unassigned-mode? 1 (count join-ids)))]}
             (when-not unassigned-mode? {:where [:in :relations.owner_id [:inline join-ids]]}))]]
    (if inverted-mode? [:not r] r)))

(defn- or-partial
  [join-ids]
  {:select :items.id
   :from [:items]
   :join [:relations [:= :items.id :relations.target_id]]
   :where [:in :relations.owner_id [:inline join-ids]]})

(defn- or-query
  [join-ids unassigned-mode?]
  (if unassigned-mode?
    [:and [:not [:in :items.id (or-partial join-ids)]]
     [:in :items.id
      {:select :items.id
       :from [:items]
       :join [:relations [:= :items.id :relations.target_id]]
       :group-by :items.id
       :having [:raw "COUNT(items.id) > 1"]}]]
    [:not [:in :items.id (or-partial join-ids)]]))

(defn- order-by
  [search-mode]
  [(if (= search-mode 5)
     [:items.inserted_at :desc]
     (if (= search-mode 4)
       [:items.date :desc]
       (if (or (= 2 search-mode) (= 3 search-mode))
         [:items.sort_idx (if (= 2 search-mode) :asc :desc)]
         [:items.updated_at (if (= 1 search-mode) :asc :desc)])))])

(defn search-related-items
  [q
   {:keys [selected-item-id join-ids search-mode unassigned-mode? inverted-mode? description-filter]
    :as _opts} {:keys [limit] :as _ctx}]
  (let [or-mode? (when join-ids inverted-mode?)]
    (-> (merge {:select (vec (concat core/select [:relations.annotation]))
                :from :items
                :where [:and
                        (when (or join-ids unassigned-mode?)
                          (if or-mode?
                            (or-query join-ids unassigned-mode?)
                            (and-query join-ids unassigned-mode? inverted-mode?)))
                        (get-search-clause q) (get-events-exist-clause search-mode)
                        (get-description-filter-clause description-filter)
                        [:= :relations.owner_id [:raw selected-item-id]]
                        (when (or (= 2 search-mode) (= 3 search-mode)) [:> :sort_idx 0])]}
               {:order-by (order-by search-mode)}
               (when limit {:limit limit})
               {:join [:relations [:= :items.id :relations.target_id]]})
        sql/format)))
