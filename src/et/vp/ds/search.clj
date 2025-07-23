(ns et.vp.ds.search
  (:require [cambium.core :as log]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [et.vp.ds.search.related-items :as related-items]
            [et.vp.ds.search.items :as items]
            [et.vp.ds.helpers
             :refer [un-namespace-keys post-process-base]
             :as helpers]))

(defn- post-process [result]
  (let [{:keys [annotation issue_annotation] :as r} (post-process-base result)]
    (cond-> r 
      (empty? annotation)
      (assoc :annotation issue_annotation))))

(defn search-items
  [db q opts]
  (when (:selected-context opts) 
    (throw (IllegalArgumentException. "Didn't expect 'selected-context' here. Did you mean to pass 'selected-context-id'?")))
  (try
    (->>
     (items/search q opts)
     (jdbc/execute! db)
     (map post-process))
    (catch Exception e
      (log/error (str "error in search/search-items: " e " - param was: " q))
      (throw e))))

(defn- do-query [db formatted-query]
  #_(prn "???" formatted-query)
  (let [issues (jdbc/execute! db formatted-query)]
    (log/info (str "count: " (count issues)))
    issues))

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
   selected-context-id 
   {:keys [link-issue search-mode] :as opts}
   {:keys [limit] :as ctx}]
  (when link-issue (throw (IllegalArgumentException. "'link-issue' shouldn't be supplied here any longer")))
  (when-not selected-context-id (throw (IllegalArgumentException. "selected-context-id must not be nil")))
  (let [opts (modify opts)
        issues (do-query db 
                         (related-items/search 
                          q
                          {:selected-context-id selected-context-id
                           :search-mode         search-mode
                           :unassigned-mode?    (:secondary-contexts-unassigned-selected opts)
                           :join-ids            (join-ids opts)
                           :inverted-mode?      (:secondary-contexts-inverted opts)}
                          ctx))
        results (map post-process (seq issues))]
    (when (and limit (> (count results) limit)) 
      (throw (Exception. "got more results than 'limit' allows. impl broken!")))
    results))

(defn search 
  "Prefer calling search-items or search-related-items"
  [db q selected-context-id {:keys [link-issue selected-context] :as opts} ctx]
  (log/info (str "search:" selected-context-id " link-issue:" link-issue))
  (when selected-context (throw (IllegalArgumentException. "'selected-context' not expected as an argument here")))
  (if selected-context-id
   (if link-issue
     (search-items db 
                   q
                   (assoc opts
                          :all-items? true
                          :selected-context-id selected-context-id))
     (search-related-items db 
                           q
                           selected-context-id
                           opts
                           ctx))
    (search-items db q (assoc opts :all-items? true))))

(defn- try-parse [item]
  (try (Integer/parseInt item)
       (catch Exception _e nil)))

(defn- pre-process-highlighted-secondary-contexts
  [highlighted-secondary-contexts]
  (->> highlighted-secondary-contexts
       (keep try-parse)))

(defn get-title
  [db {:keys [id]}] 
  (-> {:select   [:issues.title]
       :from     [:issues]
       :where    [:= :issues.id [:inline id]]
       :group-by [:issues.id]
       :order-by [[:issues.updated_at :desc]]}
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

;; what i don't completely like that this function here, in contrast
;; to the other functions in this namespace, depends on the :data 
;; inside the items, whereas the others don't (and deliberately so, after
;; the refactoring to do all the query logic as sql, as it should be
;; instead of after the fact clojure filtering).
(defn fetch-aggregated-contexts 
  [db {{{:keys [highlighted-secondary-contexts]} :data} :selected-context
       :as opts}]
  (let [issues (search-related-items 
                db 
                "" 
                (:id (:selected-context opts))
                {}
                {})]
    (->> issues
         (map #(get-in % [:data :contexts]))
         (map #(filter (fn [[_id {:keys [show-badge?]}]] show-badge?) %))
         (map seq)
         (apply concat)
         (group-by first)
         (map #(do [(count (second %)) (first (second %))]))
         (sort-by first)
         reverse
         (map (fn [[count [id title]]]
                [(Integer/parseInt (name id)) [title count]]))
         (sort-secondary-contexts db highlighted-secondary-contexts))))
