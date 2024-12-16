(ns et.vp.ds.relations
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [cambium.core :as log]
            [cheshire.core :as json]))

(defn- get-title [container]
  (or (and (string? (:short_title container))
           (not-empty (:short_title container)))
      (:title container)))

(defn set-collection-titles-of-new-issue [db item-id]
  (let [data (:issues/data (jdbc/execute-one! db
                                              (sql/format {:select [:data]
                                                           :from   [:issues]
                                                           :where  [:= :id [:inline item-id]]})
                                              {:return-keys true}))
        data (cond (nil? data) {}
                   :else (json/parse-string (.getValue data)))
        data (if (get data "contexts")
               data
               (assoc data "contexts" {}))
        contexts (dissoc (into {}
                               (map (fn [{:issues/keys [id title short_title]}]
                                      [id {:title (if (seq short_title)
                                            short_title
                                            title)
                                           :show-badge? true}]
                                      ) (jdbc/execute! db
                                                      (sql/format {:select [:issues.id :title :short_title]
                                                                   :from   [:collections]
                                                                   :join   [:issues [:= :collections.container_id :issues.id]]
                                                                   :where  [:= :collections.item_id [:inline item-id]]})
                                                      {:return-keys true})))
                         item-id)]
    (log/info (str "item-id: " item-id ". contexts: " contexts "."))
    (jdbc/execute-one! db
                       (sql/format {:update [:issues]
                                    :where  [:= :id [:inline item-id]]
                                    :set    {:data [:inline (json/generate-string (assoc data "contexts" contexts))]}})
                       {:return-keys true})))

(defn update-collection-title-in-collection-items
  "Standard use case is that you know item-id references id via contexts. That id has a new title, so we update it.
   @param constraints a list of ids; when set, the contexts of the item with item-id will be reduced to the ones present in that list
     so the use case is not to set the title in an item's context (with a given id), but to remove contexts"
  [db item-id id {:keys [short_title title new-contexts show-badge? remove-from-container?]}]
  (let [data (:issues/data (jdbc/execute-one! db
                                              (sql/format {:select [:data]
                                                           :from   [:issues]
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
                                           (assoc-in contexts [(str id) "title"]  
                                                     (if (seq short_title)
                                                       short_title
                                                       title))
                                           (assoc contexts (str id)   
                                                  {:show-badge? show-badge?
                                                   :title       (if (seq short_title)
                                                                  short_title
                                                                  title)})))))]
    (jdbc/execute-one! db
                       (sql/format {:update [:issues]
                                    :where  [:= :id [:inline item-id]]
                                    :set    {:data [:inline (json/generate-string data)]}})
                       {:return-keys true})))

(defn update-collection-title-in-collection-items-for-children 
  [db id title short_title]
  (let [item-ids (doall (map :collections/item_id
                             (jdbc/execute! db
                                            (sql/format {:select [:item_id]
                                                         :from   [:collections]
                                                         :where  [:= :container_id [:inline id]]})
                                            {:return-keys true})))]
    (doall (for [item-id item-ids]
             (update-collection-title-in-collection-items db item-id id {:short_title short_title :title title})))))

(defn- set-containers-of-item!
  [db item containers]
  (log/info (str "datastore.relations/set-containers-of-item! " (:id item) "." (:title item) "..." containers))
  (jdbc/execute! db (sql/format {:delete-from [:collections]
                                 :where [:= :item_id [:inline (:id item)]]}))
  (doall (for [[container-id {:keys [show-badge? annotation]}] containers]
           (jdbc/execute! db (sql/format {:insert-into [:collections]
                                          :columns [:item_id :container_id :annotation :show_badge]
                                          :values [[[:inline (:id item)]
                                                    [:inline container-id]
                                                    [:inline annotation]
                                                    [:inline show-badge?]]]})))))

(defn set-the-containers-of-item! 
  [db item containers is_context]
  (if (or is_context
          (seq (keys containers)))
    (do
      (set-containers-of-item! db item containers)
      (update-collection-title-in-collection-items db (:id item) nil
                                                   {:short_title nil :title nil :new-contexts containers}))
    (log/info {:is_context is_context
               :item (select-keys item [:id :title])} "cant take out the remaining context if item is not a context")))

(defn link-item-to-container!
  [db item container show-badge?]
  (let [contexts (merge (:contexts (:data item))
                        {(:id container) 
                         {:title (get-title container)
                          :show-badge? show-badge?}})]
    (set-containers-of-item! db item contexts)
    (update-collection-title-in-collection-items db 
                                                 (:id item) 
                                                 (:id container)
                                                 {:short_title (:short_title container)
                                                  :title (:title container)
                                                  :show-badge? show-badge?})))

(defn unlink-item-from-container!
  [db item container]
  (let [selected-item (update-in item [:data :contexts] #(dissoc % (:id container)))
        containers (:contexts (:data selected-item))]
    (log/info {:is_context (:is_context item)
               :containers containers} "unlink-item-from-container!")
    (if-not (or (seq (keys containers))
                (:is_context item)) 
      (do
        (log/info {:item (select-keys item [:id :title])
                   :container (select-keys item [:id :title])} "can't unlink item from container")
        false)
      (do
        (set-containers-of-item! db selected-item containers)
        (update-collection-title-in-collection-items db
                                                                         (:id selected-item)
                                                                         (:id container) 
                                                                         {:short_title nil
                                                                          :title nil
                                                                          :remove-from-container? true})
        true))))