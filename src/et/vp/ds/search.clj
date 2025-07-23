(ns et.vp.ds.search
  (:require [clojure.set :as set]
            [cambium.core :as log]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [et.vp.ds.search.related-items :as search.related-items]
            [et.vp.ds.search.items :as search.items]
            [et.vp.ds.helpers
             :refer [un-namespace-keys post-process-base]
             :as helpers]))

;; TODO this ns should be completely oblivious of the :data data-structure inside the items

(defn- post-process [result]
  (let [{:keys [annotation issue_annotation] :as r} (post-process-base result)]
    (cond-> r 
      (empty? annotation)
      (assoc :annotation issue_annotation))))

;; TODO make it take a q param, and an (optional, in the beginning) opts argument
(defn search-items
  [db opts]
  (let [opts (if (string? opts) 
               {:q opts}
               opts)
        {:keys [q]} opts]
    (try
      (->>
       (search.items/fetch-items q opts)
       (jdbc/execute! db)
       (map post-process))
      (catch Exception e
        (log/error (str "error in search/search-items: " e " - param was: " q))
        (throw e)))))

(defn- do-query [db formatted-query]
  #_(prn "???" formatted-query)
  (let [issues (jdbc/execute! db formatted-query)]
    (log/info (str "count: " (count issues)))
    issues))

(defn- no-modifiers-selected? [{:keys [secondary-contexts-unassigned-selected
                                       secondary-contexts-inverted]}]
  (not (or secondary-contexts-inverted 
           secondary-contexts-unassigned-selected)))

(defn- join-ids [selected-context]
  (let [current-view                (-> selected-context :data :views :current)
        selected-secondary-contexts (-> current-view :selected-secondary-contexts)] 
    (when (and (seq selected-secondary-contexts)  
               (or (no-modifiers-selected? current-view)
                   (:secondary-contexts-inverted current-view)))
      selected-secondary-contexts)))

(defn- do-fetch-issues 
  [db {:keys [selected-context link-issue limit force-limit?]
       :as   state} search-mode]
  (let [selected-context-id (:id selected-context)
        current-view (-> selected-context :data :views :current)
        issues (do-query db 
                         (search.related-items/fetch-items 
                          (or (:q state) "") 
                          {:selected-context-id selected-context-id
                           :search-mode         search-mode
                           :unassigned-mode?    (:secondary-contexts-unassigned-selected current-view)
                           :join-ids            (join-ids selected-context)
                           :inverted-mode?      (:secondary-contexts-inverted current-view)
                           :exclude-id? link-issue}
                          {:limit (or limit 500)
                           :force-limit? force-limit?}))]
    (seq issues)))

(defn modify [_opts selected-context]
  (when selected-context
    (let [current-view (-> selected-context :data :views :current)]
      (cond-> selected-context
        (and (seq (:selected-secondary-contexts current-view))  
             (:secondary-contexts-unassigned-selected current-view)
             (not (:secondary-contexts-inverted current-view)))
        (assoc-in  
         [:data :views :current :secondary-contexts-unassigned-selected] nil)))))

(defn- search-related-items'
  [db {{{{{:keys [search-mode]} :current} :views} :data} :selected-context
       :as opts}]
  (let [opts (update opts :selected-context (partial modify opts))]
    (->> (do-fetch-issues db opts search-mode)
         (map post-process))))

(defn search-related-items 
  [db 
   q 
   selected-context-id 
   {:keys [link-issue] :as opts}
   {:keys [_limit _force-limit?] :as ctx}]
  (when-not selected-context-id (throw (IllegalArgumentException. "selected-context-id must not be nil")))
  (let [selected-context {:id   selected-context-id
                          ;; TODO get rid of this now, our impl doesn't depend on this anymore
                          :data {:views {:current (select-keys
                                                   opts
                                                   [:secondary-contexts-inverted
                                                    :secondary-contexts-unassigned-selected
                                                    :selected-secondary-contexts
                                                    :search-mode])}}}]
    (search-related-items' db (merge {:selected-context selected-context
                              :q q}
                             (when link-issue {:link-issue link-issue})
                             ctx))))

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
  (let [issues (search-related-items' db (-> opts
                                      (assoc :q "")
                                      (assoc-in [:selected-context :data :views :current :search-mode] 0)
                                      (assoc-in [:selected-context :data :views :current :selected-secondary-contexts] [])
                                      (assoc-in [:selected-context :data :views :current :secondary-contexts-inverted] false)
                                      (assoc-in [:selected-context :data :views :current :secondary-contexts-unassigned-selected] false)))]
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
