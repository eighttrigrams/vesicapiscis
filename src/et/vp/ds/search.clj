(ns et.vp.ds.search
  (:require [cambium.core :as log]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [et.vp.ds.search.core :as core]
            [et.vp.ds.helpers
             :refer [un-namespace-keys post-process-base]
             :as helpers]
            [et.vp.ds.search :as search]))

(defn- post-process [result]
  (let [{:keys [annotation item_annotation] :as r} (post-process-base result)]
    (cond-> r 
      (empty? annotation)
      (assoc :annotation item_annotation))))

(defn- post-process-contexts [item]
  (if (-> item :data :contexts)
    (update-in item [:data :contexts] 
               (fn [contexts]
                 (into {} (map (fn [[k v]]
                                 (try
                                   [(Integer/parseInt (name k)) v]
                                   (catch Exception e
                                     (log/error {:e e
                                                 :k k
                                                 :v v
                                                 :item item} 
                                                "whoops while trying to convert to int")
                                     [k v])))
                            contexts)))) 
    item))

(comment
  (post-process-contexts {:data {:contexts {"123"  {:title       "Name1"
                                                    :show-badge? true}
                                            :456 {:title       "Name2"
                                                  :show-badge? true}}}}))

(defn search-items
  [db 
   q 
   {:keys [all-items? link-context link-item] :as opts}
   ctx]
  (when (:selected-item opts) 
    (throw (IllegalArgumentException. "Didn't expect 'selected-item' here. Did you mean to pass 'selected-item-id'?")))
  (when (and link-context all-items?)
    (throw (IllegalArgumentException. "Can't combine 'all-items?' and 'link-context'")))
  (when (and link-item (not all-items?))
    (throw (IllegalArgumentException. "Must set 'all-items?' on 'link-item'")))
  (try
    (->>
     (core/search-items q opts ctx)
     (jdbc/execute! db)
     (map post-process)
     (map post-process-contexts))
    (catch Exception e
      (log/error (str "error in search/search-items: " e " - param was: " q))
      (throw e))))

(defn- do-query [db formatted-query]
  #_(prn "???" formatted-query)
  (let [items (jdbc/execute! db formatted-query)]
    (log/info (str "count: " (count items)))
    items))

(defn- no-modifiers-selected? [{:keys [secondary-contexts-unassigned-selected
                                       secondary-contexts-inverted]}]
  (not (or secondary-contexts-inverted 
           secondary-contexts-unassigned-selected)))

(defn- join-ids [opts]
  (let [selected-secondary-contexts (:selected-secondary-contexts opts)] 
    (when (and (seq selected-secondary-contexts)  
               (or (no-modifiers-selected? opts)
                   (:secondary-contexts-inverted opts)))
      selected-secondary-contexts)))

(defn modify [opts]
  (cond-> opts
    (and (seq (:selected-secondary-contexts opts))  
         (:secondary-contexts-unassigned-selected opts)
         (not (:secondary-contexts-inverted opts)))
    (assoc :secondary-contexts-unassigned-selected nil)))

(defn search-related-items
  [db 
   q 
   selected-item-id 
   {:keys [link-item search-mode] :as opts}
   {:keys [limit] :as ctx}]
  (when link-item (throw (IllegalArgumentException. "'link-item' shouldn't be supplied here any longer")))
  (when-not selected-item-id (throw (IllegalArgumentException. "selected-context-id must not be nil")))
  (let [opts (modify opts)
        items (do-query db 
                         (core/search-related-items
                          q
                          {:selected-item-id selected-item-id
                           :search-mode         search-mode
                           :unassigned-mode?    (:secondary-contexts-unassigned-selected opts)
                           :join-ids            (join-ids opts)
                           :inverted-mode?      (:secondary-contexts-inverted opts)}
                          ctx))
        results (->> (seq items)
                     (map post-process)
                     (map post-process-contexts))]
    (when (and limit (> (count results) limit)) 
      (throw (Exception. "got more results than 'limit' allows. impl broken!")))
    results))

(defn- try-parse [item]
  (try (Integer/parseInt item)
       (catch Exception _e nil)))

(defn- pre-process-highlighted-secondary-contexts
  [highlighted-secondary-contexts]
  (->> highlighted-secondary-contexts
       (keep try-parse)))

(defn get-title
  [db {:keys [id]}] 
  (-> {:select   [:items.title]
       :from     [:items]
       :where    [:= :items.id [:inline id]]
       :group-by [:items.id]
       :order-by [[:items.updated_at :desc]]}
      sql/format
      (#(jdbc/execute-one! db % {:return-keys true}))
      un-namespace-keys
      :title))

(defn- calc-highlighted [db 
                         secondary-contexts
                         highlighted-secondary-contexts]
  (reduce (fn [acc val]
            (if (secondary-contexts val)
              (conj acc [val (conj (secondary-contexts val) true)])
              (if-let [title (get-title db {:id val})]
                (conj acc [val [title 0 true]])
                acc)))
          [] highlighted-secondary-contexts))

(defn- sort-secondary-contexts
  [db highlighted-secondary-contexts secondary-contexts]
  (let [highlighted-secondary-contexts (pre-process-highlighted-secondary-contexts
                                        highlighted-secondary-contexts)
        secondary-contexts             (into {} secondary-contexts)
        front                          (calc-highlighted db 
                                                         secondary-contexts 
                                                         highlighted-secondary-contexts)
        back                           (->> secondary-contexts
                                            (remove (fn [[k _v]]
                                                      (some #{k} highlighted-secondary-contexts)))
                                            (map (fn [[k [val title]]] [k [val title false]])))]
    (concat front (reverse (sort-by #(get-in % [1 1]) back)))))

(defn get-aggregated-contexts
  [db items highlighted-secondary-contexts]
  (->> items
       (map #(get-in % [:data :contexts]))
       (map #(filter (fn [[_id {:keys [show-badge? is-context?]}]] (and show-badge? is-context?)) %))
       (map seq)
       (apply concat)
       (group-by first)
       (map #(do [(count (second %)) (first (second %))]))
       (sort-by first)
       reverse
       (map (fn [[count [id title]]] [id [title count]]))
       (sort-secondary-contexts db highlighted-secondary-contexts)))
