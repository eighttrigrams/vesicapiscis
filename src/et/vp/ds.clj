(ns et.vp.ds
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [cheshire.core :as json]
            [cambium.core :as log]
            [et.vp.ds.relations :as datastore.relations]
            [et.vp.ds.helpers
             :refer [un-namespace-keys post-process-base]
             :as helpers]))

(defn delete-date [db issue-id]
  (jdbc/execute! db
                 (sql/format {:update [:issues]
                       :set    {:date nil}
                       :where [:= :id [:inline issue-id]]})))

(defn insert-date [db issue-id date]
  (jdbc/execute! db (sql/format {:update [:issues]
                                 :set    {:date [:inline date]}
                                 :where [:= :id [:inline issue-id]]})))

(defn- update-contexts [item]
  (let [m (if (:collections_id item)
            (zipmap (.getArray (:collections_id item))
                    (.getArray (:collections_annotation item)))
            {})]
    (->
     item
     (update-in [:data :contexts] 
                (fn [contexts]
                  (into {} 
                        (map (fn [[k v]]
                               (let [id (Integer/parseInt (name k))]
                                 [id
                                  (if (map? v)
                                    (assoc v :annotation (get m id))
                                    {:title       v
                                     :annotation nil
                                     :show-badge? true})]))
                             contexts))))
     (dissoc :collections_id :collections_annotation))))

(comment
  (update-contexts {:data {:contexts {"123" "Name"
                                      "456" {:title "Name" :show-badge? true}}}}))

(defn- post-process [query-result]
  (-> query-result
      post-process-base
      update-contexts))

(declare get-item)

(defn switch-between-issue-and-context! [db {:keys [id is_context] :as item}]
  (let [contexts (-> item :data :contexts)]
    (if (or
           (not is_context) 
           (seq contexts))
      (jdbc/execute-one! db
                         (sql/format {:update [:issues]
                                      :where  [:= :id [:inline id]]
                                      :set    {:is_context (not is_context)
                                               :updated_at_ctx [:raw "NOW()"]
                                               :updated_at  [:raw "NOW()"]}})
                         {:return-keys true})
      (log/info {:has-contexts? (seq contexts)
                 :is-context? is_context} "can't flip context"))
    (get-item db item)))

(defn get-contained-items-count [db id]
  (count (jdbc/execute! db
                        (sql/format {:select :*
                                     :from   [:collections]
                                     :where  [:= :container_id id]})
                        {:return-keys true})))

(defn delete-item
  [db {:keys [id]}]
  (delete-date db id)
  (jdbc/execute! db (sql/format {:delete-from [:collections]
                                 :where [:= :item_id [:inline id]]}))
  (jdbc/execute! db (sql/format {:delete-from [:issues]
                                 :where [:= :id [:inline id]]})))

(declare get-item)

(defn- basic-issues-query [id]
  {:select   [:issues.*
              [[:array_agg :collections.container_id] :collections_id]
              [[:array_agg :collections.annotation] :collections_annotation]]
   :from     [:issues]
   :join     [:collections [:= :issues.id :collections.item_id]]
   :where    [:= :issues.id [:inline id]]
   :group-by [:issues.id]
   :order-by [[:issues.updated_at :desc]]})

(defn- simple-issues-query [id]
  {:select   [:issues.*]
   :from     [:issues]
   :where    [:= :issues.id [:inline id]]
   :group-by [:issues.id]
   :order-by [[:issues.updated_at :desc]]})

(defn- get-issue-without-related-issues [db id]
  (or (-> (basic-issues-query id)
          sql/format
          (#(jdbc/execute-one! db % {:return-keys true})))
      (-> (simple-issues-query id)
          sql/format
          (#(jdbc/execute-one! db % {:return-keys true})))))

(defn get-item
  [db {:keys [id]}]
  (-> (get-issue-without-related-issues db id)
        post-process)
  #_(try
    (-> (get-issue-without-related-issues db id)
        post-process)
    #_(catch java.lang.Exception e
      (prn "get-issue-----" (.getMessage e))
      (throw e))))

(defn- basic-title-query [title]
  {:select   [:issues.*]
   :from     [:issues]
   :where    [:= :issues.title [:inline title]]
   :group-by [:issues.id] ;; TODO remove
   :order-by [[:issues.updated_at :desc]]})

(defn- get-issue-without-related-issues-by-title [db id]
  (-> (basic-title-query id)
      sql/format
      (#(jdbc/execute-one! db % {:return-keys true}))))

(defn get-item-by-title 
  [db {:keys [title]}]
  (-> (get-issue-without-related-issues-by-title db title)
      post-process
      (assoc :contexts {})
      (assoc :related_issues {})))

(defn- basic-find-query [path match]
  {:select   [:issues.*]
   :from     [:issues]
   :where    [:= path [:inline match]]})

(defn- get-issue-without-related-issues-by-path [db path url]
  (-> (basic-find-query [:raw path] url)
      sql/format
      (#(jdbc/execute-one! db % {:return-keys true}))))

(defn get-item-by-path
  [db path url]
  (try
    (-> (get-issue-without-related-issues-by-path db path url)
        post-process
        (assoc :contexts {}))
    (catch java.lang.Exception e
      (throw e))))

(defn get-items-by-path [db path url]
  (-> (basic-find-query [:raw path] url)
      sql/format
      (#(jdbc/execute! db % {:return-keys true}))))

(defn- update-item' [db {:keys [id title short_title tags data] :as item}]
  (log/info "update-item!!!!!!!!!")
  (let [old-item      (get-item db item)
        old-data      (:data old-item)
        data          (if data
                        (merge old-data data)
                        {})
        data (if (and (:contexts data) (map? (:contexts data)))
               (update data :contexts (fn [contexts]
                                        (->> contexts
                                             (map (fn [[k v]] 
                                                    [k (dissoc v :annotation)]))
                                             (into {}))))
               data)
        set           (merge {:title       [:inline title]
                              :short_title [:inline short_title]
                              :tags        [:inline tags]}
                             (merge {:data [:inline (json/generate-string data)]}))
        formatted-sql (sql/format {:update [:issues]
                                   :where  [:= :id [:inline id]]
                                   :set    set})
        _result       (jdbc/execute-one! db
                                         formatted-sql
                                         {:return-keys true})]
    (or (not= (:title old-item) title)
        (not= (:short_title old-item) short_title))))

(defn update-item [db {:keys [id title short_title date] :as item}]
  (delete-date db id)
  (when date
    (insert-date db id date))
  (let [has-title-changed? (update-item' db item)]
    (when has-title-changed?
      (future
        (try
          (datastore.relations/update-collection-title-in-collection-items-for-children db id title short_title)
          (catch Exception e
            (log/error (.getMessage e))))))) 
  (get-item db item))

(defn update-context-description [db {:keys [id description]}]
  (jdbc/execute-one! db
                     (sql/format {:update [:issues]
                                  :set    {:description    [:inline description]
                                           :updated_at_ctx [:raw "NOW()"]}
                                  :where  [:= :id [:inline id]]})
                     {:return-keys true})
  (get-item db {:id id}))

(defn store-current-view [db {:keys [id] :as selected-context} {:keys [title]}]
  (let [data (:data (get-item db selected-context))
        data (update-in data [:views :stored] conj {:title title
                                                    :view (:current (:views data))})]
    (jdbc/execute-one! db (sql/format {:update [:issues]
                                       :set    {:data [:inline (json/generate-string data)]}
                                       :where  [:= :id [:inline id]]}))) 
  (get-item db selected-context))

(defn load-stored-context [db {:keys [id] :as selected-context} idx]
  (let [data (:data (get-item db selected-context))
        data (assoc-in data [:views :current] 
                       (-> data :views :stored (get idx) :view))]
    (jdbc/execute-one! db (sql/format {:update [:issues]
                                       :set    {:data [:inline (json/generate-string data)]}
                                       :where  [:= :id [:inline id]]})))
  (get-item db selected-context))

;; https://stackoverflow.com/a/18319708
(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn remove-stored-context [db {:keys [id] :as selected-context} idx]
  (let [data (:data (get-item db selected-context))
        data (update-in data [:views :stored] #(vec-remove idx %))]
    (jdbc/execute-one! db (sql/format {:update [:issues]
                                       :set    {:data [:inline (json/generate-string data)]}
                                       :where  [:= :id [:inline id]]})))
  (get-item db selected-context))

(defn cycle-search-mode [db {:keys [id] :as context}]
  (let [data (-> (get-item db context)
                 :data
                 (update-in [:views :current :search-mode]
                            #(mod (inc (or % 0)) 6)))]
    (jdbc/execute-one! db (sql/format {:update [:issues]
                                       :set    {:data [:inline (json/generate-string data)]}
                                       :where  [:= :id [:inline id]]}))
    (get-item db context)))

(defn reprioritize-context [db {:keys [id]}]
  (jdbc/execute! db (sql/format {:update [:issues]
                                 :set {:updated_at_ctx [:raw "NOW()"]}
                                 :where [:= :id [:inline id]]})))

(defn reprioritize-issue [db {:keys [id]}]
  (jdbc/execute! db (sql/format {:update [:issues]
                                 :set {:updated_at [:raw "NOW()"]}
                                 :where [:= :id [:inline id]]})))

(defn- create-new-issue! [db title short_title suppress-digit-check?]
  (let [now (helpers/gen-date)]
    (:issues/id (jdbc/execute-one!
                 db
                 (sql/format {:insert-into [:issues]
                              :columns     [:inserted_at
                                            :updated_at
                                            :updated_at_ctx
                                            :title
                                            :short_title]
                              :values      [[[:raw now]
                                             [:raw now]
                                             [:raw now]
                                             title
                                             (if (and (not suppress-digit-check?)
                                                      (boolean (re-find #"\d" short_title)))
                                               (do 
                                                 (log/error (str "Can't insert short_title due to it containing digit: " short_title))
                                                 "")
                                               short_title)]]})

                 {:return-keys true}))))

(defn- insert-issue-relations! [db values]
  (jdbc/execute! db
                 (sql/format {:insert-into [:collections]
                              :columns     [:container_id :item_id]
                              :values      values})))

(defn new-issue 
  ([db title short-title context-ids-set]
   (new-issue db title short-title context-ids-set {}))
  ([db 
    title
    short-title
    context-ids-set
    {:keys [suppress-digit-check?]}]
   (when-not (seq context-ids-set) 
     (throw (Exception. "won't create a new-issue when no contexts")))
   (let [issue-id (create-new-issue! db title short-title suppress-digit-check?)
         values   (vec (doall
                        (map (fn [ctx-id]
                               [[:inline ctx-id]
                                [:inline issue-id]])
                             context-ids-set)))]
     (insert-issue-relations! db values)
     (get-item db {:id issue-id}))))

(defn new-context [db {title :title}]
  (let [now (helpers/gen-date)]
    (-> (jdbc/execute-one!
         db
         (sql/format {:insert-into [:issues]
                      :columns     [:inserted_at
                                    :updated_at
                                    :updated_at_ctx
                                    :title
                                    :is_context]
                      :values      [[[:raw now] ;; before, it was just "NOW()"
                                     [:raw now]
                                     [:raw now]
                                     [:inline title]
                                     true]]})
         {:return-keys true})
        un-namespace-keys
        (dissoc :searchable))))
