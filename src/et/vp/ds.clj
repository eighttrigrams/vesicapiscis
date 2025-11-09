(ns et.vp.ds
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [cheshire.core :as json]
            [cambium.core :as log]
            [et.vp.ds.relations :as datastore.relations]
            [et.vp.ds.helpers :refer [un-namespace-keys post-process-base] :as helpers]))

(defn delete-date
  [db item-id]
  (jdbc/execute! db
                 (sql/format
                  {:update [:items] :set {:date nil} :where [:= :id [:inline item-id]]})))

(defn insert-date
  [db item-id date]
  (jdbc/execute!
   db
   (sql/format {:update [:items] :set {:date [:inline date]} :where [:= :id [:inline item-id]]})))

(defn- update-contexts
  [item]
  (let [m (if (:relations_id item)
            (zipmap (.getArray (:relations_id item)) (.getArray (:relations_annotation item)))
            {})]
    (-> item
        (update-in [:data :contexts]
                   (fn [contexts]
                     (into {}
                           (keep (fn [[k v]]
                                   (when-let [id (try (Integer/parseInt (name k))
                                                      (catch Exception e
                                                        (log/error {:e e :item item :k k :v v})
                                                        nil))]
                                     [id
                                      (if (map? v)
                                        (assoc v :annotation (get m id))
                                        {:title v :annotation nil :show-badge? true})]))
                                 contexts))))
        (dissoc :relations_id :relations_annotation))))

(comment
  (update-contexts {:data {:contexts {"123" "Name" "456" {:title "Name" :show-badge? true}}}}))

(defn- post-process
  [query-result]
  (-> query-result
      post-process-base
      update-contexts))

(declare get-item)

(defn switch-between-item-and-context!
  [db {:keys [id is_context] :as item}]
  (log/info {:id id :is_context is_context} "switch-between-item-and-context!")
  (let [contexts (-> item
                     :data
                     :contexts)]
    (if (or (not is_context) (seq contexts))
      (do (log/info "Update the item's is_context status")
          (jdbc/execute-one! db
                             (sql/format {:update [:items]
                                          :where [:= :id [:inline id]]
                                          :set {:is_context (not is_context)
                                                :updated_at_ctx [:raw "NOW()"]
                                                :updated_at [:raw "NOW()"]}})
                             {:return-keys true})
          (log/info "Update all items that have this item in their contexts")
          (let [related-items (jdbc/execute! db
                                             (sql/format {:select [:target_id]
                                                          :from [:relations]
                                                          :where [:= :owner_id [:inline id]]})
                                             {:return-keys true})]
            (log/info "Updating related items")
            (doseq [{:relations/keys [target_id]} related-items]
              (log/info {:target_id target_id} "Updating related item")
              (datastore.relations/update-collection-title-in-collection-items
               db
               target_id
               id
               {:is-context? (not is_context)
                :title (:title item)
                :short_title (:short_title item)}))))
      (log/info {:has-contexts? (seq contexts) :is-context? is_context} "can't flip context"))
    (get-item db item)))

(defn get-contained-items-count
  [db id]
  (count (jdbc/execute! db
                        (sql/format {:select :* :from [:relations] :where [:= :owner_id id]})
                        {:return-keys true})))

(defn delete-item
  [db {:keys [id]}]
  (delete-date db id)
  (jdbc/execute! db (sql/format {:delete-from [:relations] :where [:= :target_id [:inline id]]}))
  (jdbc/execute! db (sql/format {:delete-from [:items] :where [:= :id [:inline id]]})))

(declare get-item)

(defn- basic-items-query
  [id]
  {:select [:items.* [[:array_agg :relations.owner_id] :relations_id]
            [[:array_agg :relations.annotation] :relations_annotation]]
   :from [:items]
   :join [:relations [:= :items.id :relations.target_id]]
   :where [:= :items.id [:inline id]]
   :group-by [:items.id]
   :order-by [[:items.updated_at :desc]]})

(defn- simple-items-query
  [id]
  {:select [:items.*]
   :from [:items]
   :where [:= :items.id [:inline id]]
   :group-by [:items.id]
   :order-by [[:items.updated_at :desc]]})

(defn- get-item-without-related-items
  [db id]
  (or (-> (basic-items-query id)
          sql/format
          (#(jdbc/execute-one! db % {:return-keys true})))
      (-> (simple-items-query id)
          sql/format
          (#(jdbc/execute-one! db % {:return-keys true})))))

(defn get-item
  [db {:keys [id]}]
  (-> (get-item-without-related-items db id)
      post-process))

(defn- basic-title-query
  [title]
  {:select [:items.*]
   :from [:items]
   :where [:= :items.title [:inline title]]
   :group-by [:items.id] ;; TODO remove
   :order-by [[:items.updated_at :desc]]})

(defn- get-item-without-related-items-by-title
  [db id]
  (-> (basic-title-query id)
      sql/format
      (#(jdbc/execute-one! db % {:return-keys true}))))

(defn get-item-by-title
  [db {:keys [title]}]
  (-> (get-item-without-related-items-by-title db title)
      post-process
      (assoc :contexts {})
      (assoc :related_items {})))

(defn- basic-find-query
  [path match]
  {:select [:items.*] :from [:items] :where [:= path [:inline match]]})

(defn- get-item-without-related-items-by-path
  [db path url]
  (-> (basic-find-query [:raw path] url)
      sql/format
      (#(jdbc/execute-one! db % {:return-keys true}))))

(defn get-item-by-path
  [db path url]
  (try (-> (get-item-without-related-items-by-path db path url)
           post-process
           (assoc :contexts {}))
       (catch java.lang.Exception e (throw e))))

(defn get-items-by-path
  [db path url]
  (-> (basic-find-query [:raw path] url)
      sql/format
      (#(jdbc/execute! db % {:return-keys true}))))

(defn- save-description-to-history
  [db id description]
  (when (and description (not (clojure.string/blank? description)))
    (let [max-version-result (jdbc/execute-one! db
                                                (sql/format {:select [[[:coalesce [:max :version] 0]
                                                                       :max_version]]
                                                             :from [:history]
                                                             :where [:= :id [:inline id]]})
                                                {:return-keys true})
          new-version (inc (:max_version max-version-result))
          _ (jdbc/execute-one! db
                               (sql/format {:insert-into [:history]
                                            :values [{:id [:inline id]
                                                      :text [:inline description]
                                                      :version [:inline new-version]}]})
                               {:return-keys true})
          history-count (:count (jdbc/execute-one! db
                                                   (sql/format {:select [[[:count :*] :count]]
                                                                :from [:history]
                                                                :where [:= :id [:inline id]]})
                                                   {:return-keys true}))]
      (when (> history-count 5)
        (jdbc/execute! db
                       (sql/format {:delete-from [:history]
                                    :where [:and [:= :id [:inline id]]
                                            [:in :version
                                             {:select [:version]
                                              :from [:history]
                                              :where [:= :id [:inline id]]
                                              :order-by [[:version :asc]]
                                              :limit (- history-count 5)}]]})))))
  nil)

(defn get-description-history
  [db {:keys [id]}]
  (let [current-item (get-item db {:id id})
        current-description (:description current-item)
        history-items (jdbc/execute! db
                                     (sql/format {:select [:text :version :created_at]
                                                  :from [:history]
                                                  :where [:= :id [:inline id]]
                                                  :order-by [[:version :desc]]})
                                     {:return-keys true})
        history-items-as-maps (map (fn [row]
                                     {:text (:history/text row)
                                      :version (:history/version row)
                                      :created_at (:history/created_at row)})
                                   history-items)
        all-versions (if (and current-description (not (clojure.string/blank? current-description)))
                       (concat [{:text current-description
                                 :version (inc (or (:history/version (first history-items)) 0))
                                 :created_at (:updated_at_ctx current-item)
                                 :current true}]
                               history-items-as-maps)
                       history-items-as-maps)]
    {:versions all-versions :total (count all-versions)}))

(defn- update-item'
  [db {:keys [id title short_title annotation sort_idx tags data] :as item}]
  (log/info (str "update-item!!!!!!!!!" title ":" sort_idx "<-" (integer? sort_idx)))
  (let [old-item (get-item db item)
        old-data (:data old-item)
        data (if data
               ;; This will prevent any attempts of a dissoc, btw. Not sure if I wanted that.
               (merge old-data data)
               {})
        data (if (and (:contexts data) (map? (:contexts data)))
               (update data
                       :contexts
                       (fn [contexts]
                         (->> contexts
                              (map (fn [[k v]] [k (dissoc v :annotation)]))
                              (into {}))))
               data)
        set (merge {:title [:inline title]
                    :short_title [:inline short_title]
                    :annotation [:inline annotation]
                    :tags [:inline tags]
                    :data [:inline (json/generate-string data)]}
                   (when sort_idx
                     {:sort_idx
                      [:inline
                       (when sort_idx
                         (if
                           ;; i think this is for when we are in tests or something
                           (integer? sort_idx)
                           sort_idx
                           (try
                             (Integer/parseInt sort_idx)
                             (catch Exception e
                               (log/error (str "This is bad ----- conversion failed" (.getMessage e)
                                               "-" (:sort_idx old-item)))
                               (if (integer? (:sort_idx old-item)) (:sort_idx old-item) -1)))))]}))
        formatted-sql (sql/format {:update [:items] :where [:= :id [:inline id]] :set set})
        _result (jdbc/execute-one! db formatted-sql {:return-keys true})]
    (or (not= (:title old-item) title) (not= (:short_title old-item) short_title))))

(defn update-item
  "NOTE that dissoc on data items won't work as there is a merge of the old with the new data going on."
  [db {:keys [id title short_title date] :as item}]
  (delete-date db id)
  (when date (insert-date db id date))
  (let [has-title-changed? (update-item' db item)]
    (when has-title-changed?
      (future (try (datastore.relations/update-collection-title-in-collection-items-for-children
                    db
                    id
                    title
                    short_title)
                   (catch Exception e (log/error (.getMessage e)))))))
  (get-item db item))

(defn update-context-description
  [db {:keys [id description]}]
  (let [old-item (get-item db {:id id})
        old-description (:description old-item)]
    (save-description-to-history db id old-description)
    (jdbc/execute-one! db
                       (sql/format {:update [:items]
                                    :set {:description [:inline description]
                                          :updated_at_ctx [:raw "NOW()"]}
                                    :where [:= :id [:inline id]]})
                       {:return-keys true})
    (get-item db {:id id})))

(defn store-current-view
  [db {:keys [id] :as selected-item} {:keys [title]}]
  (let [data (:data (get-item db selected-item))
        data (update-in data [:views :stored] conj {:title title :view (:current (:views data))})]
    (jdbc/execute-one! db
                       (sql/format {:update [:items]
                                    :set {:data [:inline (json/generate-string data)]}
                                    :where [:= :id [:inline id]]})))
  (get-item db selected-item))

(defn load-stored-context
  [db {:keys [id] :as selected-item} idx]
  (let [data (:data (get-item db selected-item))
        data (assoc-in data
              [:views :current]
              (-> data
                  :views
                  :stored
                  (get idx)
                  :view))]
    (jdbc/execute-one! db
                       (sql/format {:update [:items]
                                    :set {:data [:inline (json/generate-string data)]}
                                    :where [:= :id [:inline id]]})))
  (get-item db selected-item))

;; https://stackoverflow.com/a/18319708
(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn remove-stored-context
  [db {:keys [id] :as selected-item} idx]
  (let [data (:data (get-item db selected-item))
        data (update-in data [:views :stored] #(vec-remove idx %))]
    (jdbc/execute-one! db
                       (sql/format {:update [:items]
                                    :set {:data [:inline (json/generate-string data)]}
                                    :where [:= :id [:inline id]]})))
  (get-item db selected-item))

(defn cycle-search-mode
  [db {:keys [id] :as context}]
  (let [data (-> (get-item db context)
                 :data
                 (update-in [:views :current :search-mode] #(mod (inc (or % 0)) 6)))]
    (jdbc/execute-one! db
                       (sql/format {:update [:items]
                                    :set {:data [:inline (json/generate-string data)]}
                                    :where [:= :id [:inline id]]}))
    (get-item db context)))

(defn reprioritize-context
  [db {:keys [id]}]
  (jdbc/execute! db
                 (sql/format {:update [:items]
                              :set {:updated_at_ctx [:raw "NOW()"]}
                              :where [:= :id [:inline id]]})))

(defn reprioritize-item
  [db {:keys [id]}]
  (jdbc/execute!
   db
   (sql/format {:update [:items] :set {:updated_at [:raw "NOW()"]} :where [:= :id [:inline id]]})))

(defn- create-new-item!
  ([db title short_title] (create-new-item! db title short_title nil))
  ([db title short_title sort_idx]
   (let [now (helpers/gen-date)
         id (:items/id (jdbc/execute-one! db
                                          (sql/format
                                           {:insert-into [:items]
                                            :columns (concat [:inserted_at :updated_at
                                                              :updated_at_ctx :title :short_title]
                                                             (if sort_idx [:sort_idx] []))
                                            :values [(concat [[:raw now] [:raw now] [:raw now] title
                                                              short_title]
                                                             (if sort_idx [sort_idx] []))]})
                                          {:return-keys true}))]
     (when (empty? title) (insert-date db id (helpers/gen-iso-simple-date-str)))
     id)))

(defn- insert-item-relations!
  [db values]
  (jdbc/execute! db
                 (sql/format
                  {:insert-into [:relations] :columns [:owner_id :target_id] :values values})))

(defn new-item
  [db title short-title context-ids-set sort-idx]
  (when-not (seq context-ids-set) (throw (Exception. "won't create a new-item when no contexts")))
  (let [item-id (create-new-item! db title short-title sort-idx)
        values (vec (doall (map (fn [ctx-id] [[:inline ctx-id] [:inline item-id]])
                                context-ids-set)))]
    (insert-item-relations! db values)
    (datastore.relations/set-collection-titles-of-new-item db item-id)
    (get-item db {:id item-id})))

(defn new-context
  [db {title :title}]
  (let [now (helpers/gen-date)]
    (-> (jdbc/execute-one! db
                           (sql/format {:insert-into [:items]
                                        :columns [:inserted_at :updated_at :updated_at_ctx :title
                                                  :is_context]
                                        :values [[[:raw now] ;; before, it was just "NOW()"
                                                  [:raw now] [:raw now] [:inline title] true]]})
                           {:return-keys true})
        un-namespace-keys
        (dissoc :searchable))))
