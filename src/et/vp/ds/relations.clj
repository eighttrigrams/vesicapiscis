(ns et.vp.ds.relations
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [cambium.core :as log]
            [cheshire.core :as json]))

(defn- get-title [container]
  (or (and (string? (:short_title container))
           (not-empty (:short_title container)))
      (:title container)))

(defn set-collection-titles-of-new-item [db item-id]
  (let [data (:items/data (jdbc/execute-one! db
                                              (sql/format {:select [:data]
                                                           :from   [:items]
                                                           :where  [:= :id [:inline item-id]]})
                                              {:return-keys true}))
        data (cond (nil? data) {}
                   :else (json/parse-string (.getValue data)))
        data (if (get data "contexts")
               data
               (assoc data "contexts" {}))
        contexts (dissoc (into {}
                               (map (fn [{:items/keys [id title short_title is_context]}]
                                      [id {:title (if (seq short_title)
                                            short_title
                                            title)
                                           :show-badge? true
                                           :is-context? (boolean is_context)}]
                                      ) (jdbc/execute! db
                                                      (sql/format {:select [:items.id :title :short_title :is_context]
                                                                   :from   [:relations]
                                                                   :join   [:items [:= :relations.owner_id :items.id]]
                                                                   :where  [:= :relations.target_id [:inline item-id]]})
                                                      {:return-keys true})))
                         item-id)]
    (log/info (str "item-id: " item-id ". contexts: " contexts "."))
    (jdbc/execute-one! db
                       (sql/format {:update [:items]
                                    :where  [:= :id [:inline item-id]]
                                    :set    {:data [:inline (json/generate-string (assoc data "contexts" contexts))]}})
                       {:return-keys true})))

(defn update-collection-title-in-collection-items
  "Standard use case is that you know item-id references id via contexts. That id has a new title, so we update it.
   @param constraints a list of ids; when set, the contexts of the item with item-id will be reduced to the ones present in that list
     so the use case is not to set the title in an item's context (with a given id), but to remove contexts"
  [db item-id id {:keys [short_title title new-contexts show-badge? remove-from-container? is-context?]}]
  (let [data (:items/data (jdbc/execute-one! db
                                              (sql/format {:select [:data]
                                                           :from   [:items]
                                                           :where  [:= :id [:inline item-id]]})
                                              {:return-keys true}))
        data (cond (nil? data) {}
                   :else (json/parse-string (.getValue data)))
        data (if (get data "contexts")
               data
               (assoc data "contexts" {}))
        data (update data "contexts" (fn [contexts]
                                       (cond
                                         remove-from-container?
                                         (dissoc contexts (str id))
                                         (map? new-contexts)
                                         new-contexts
                                         :else
                                         (if (map? (get contexts (str id)))
                                           (-> contexts
                                               (assoc-in [(str id) "title"]  
                                                         (if (seq short_title)
                                                           short_title
                                                           title))
                                               (cond-> (not (nil? is-context?)) 
                                                 (assoc-in [(str id) "is-context?"] is-context?)))
                                           (assoc contexts (str id)   
                                                  (cond-> {:show-badge? show-badge?
                                                           :title       (if (seq short_title)
                                                                          short_title
                                                                          title)}
                                                    (not (nil? is-context?)) (assoc :is-context? is-context?)))))))]
    (jdbc/execute-one! db
                       (sql/format {:update [:items]
                                    :where  [:= :id [:inline item-id]]
                                    :set    {:data [:inline (json/generate-string data)]}})
                       {:return-keys true})))

(defn update-collection-title-in-collection-items-for-children 
  [db id title short_title]
  (let [item-ids (doall (map :relations/target_id
                             (jdbc/execute! db
                                            (sql/format {:select [:target_id]
                                                         :from   [:relations]
                                                         :where  [:= :owner_id [:inline id]]})
                                            {:return-keys true})))]
    (doall (for [item-id item-ids]
             (update-collection-title-in-collection-items db item-id id {:short_title short_title :title title})))))

(defn- set-containers-of-item!
  [db item containers]
  (log/info (str "datastore.relations/set-containers-of-item! " (:id item) "." (:title item) "..." containers))
  (jdbc/execute! db (sql/format {:delete-from [:relations]
                                 :where [:= :target_id [:inline (:id item)]]}))
  (doall (for [[container-id {:keys [show-badge? annotation]}] containers]
           (jdbc/execute! db (sql/format {:insert-into [:relations]
                                          :columns [:target_id :owner_id :annotation :show_badge]
                                          :values [[[:inline (:id item)]
                                                    [:inline container-id]
                                                    [:inline annotation]
                                                    [:inline show-badge?]]]})))))

(defn set-the-containers-of-item! 
  "@param containers - map {:container-id {:annotation \"annotation\"
                                           :show-badge? true|false}}"
  [db item containers is_context]
  (if (or is_context
          (seq (keys containers)))
    (do
      (set-containers-of-item! db item containers)
      (update-collection-title-in-collection-items db (:id item) nil
                                                   {:short_title nil :title nil :new-contexts containers}))
    (log/info {:is_context is_context
               :item (select-keys item [:id :title])} "cant take out the remaining context if item is not a context")))

(defn link-item-to-another-item!
  [db item another-item show-badge?]
  (let [contexts (merge (:contexts (:data item))
                        {(:id another-item) 
                         {:title (get-title another-item)
                          :show-badge? show-badge?
                          :is-context? (boolean (:is_context another-item))}})]
    (set-containers-of-item! db item contexts)
    (update-collection-title-in-collection-items db 
                                                 (:id item) 
                                                 (:id another-item)
                                                 {:short_title (:short_title another-item)
                                                  :title (:title another-item)
                                                  :show-badge? show-badge?
                                                  :is-context? (boolean (:is_context another-item))})))

(defn unlink-item-from-another-item!
  [db item another-item]
  (let [selected-item (update-in item [:data :contexts] #(dissoc % (:id another-item)))
        containers (:contexts (:data selected-item))]
    (log/info {:is_context (:is_context item)
               :containers containers} "unlink-item-from-another-item!")
    (if-not (or (seq (keys containers))
                (:is_context item))
      (do
        (log/info {:item (select-keys item [:id :title])
                   :container (select-keys item [:id :title])} "can't unlink item from another item")
        false)
      (do
        (set-containers-of-item! db selected-item containers)
        (update-collection-title-in-collection-items
         db
         (:id selected-item)
         (:id another-item)
         {:short_title            nil
          :title                  nil
          :remove-from-container? true})
        true))))

(defn update-relation-annotation!
  [db item-id context-id annotation]
  (jdbc/execute-one! db
                     (sql/format {:update [:relations]
                                  :set {:annotation [:inline annotation]}
                                  :where [:and
                                          [:= :target_id [:inline item-id]]
                                          [:= :owner_id [:inline context-id]]]})))