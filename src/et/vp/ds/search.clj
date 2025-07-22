(ns et.vp.ds.search
  (:require [clojure.set :as set]
            [cambium.core :as log]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [et.vp.ds.search.related-items :as search.related-items]
            [et.vp.ds.search.contexts :as search.contexts]
            [et.vp.ds.helpers
             :refer [un-namespace-keys post-process-base]
             :as helpers]))

;; TODO this ns should be completely oblivious of the :data data-structure inside the items

(defn- post-process [result]
  (let [{:keys [annotation issue_annotation] :as r} (post-process-base result)]
    (cond-> r 
      (empty? annotation)
      (assoc :annotation issue_annotation))))

(defn search-items
  [db opts]
  (let [opts (if (string? opts) 
               {:q opts}
               opts)
        {:keys [q]} opts]
    (try
      (->>
       (search.contexts/fetch-items q opts)
       (jdbc/execute! db)
       (map post-process))
      (catch Exception e
        (log/error (str "error in search/search-contexts: " e " - param was: " q))
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

(defn- search-issues'
  [db {{{{{:keys [search-mode]} :current} :views} :data} :selected-context
       :as opts}]
  (let [opts (update opts :selected-context (partial modify opts))]
    (->> (do-fetch-issues db opts search-mode)
         (map post-process))))

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

(defn- get-aggregated-contexts 
  [db 
   opts 
   highlighted-secondary-contexts]
  (let [issues (search-issues' db (-> opts
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
         (map (fn [[count [id title]]] [id [title count]]))
         (sort-secondary-contexts db highlighted-secondary-contexts))))

(defn fetch-aggregated-contexts [db {{{:keys [highlighted-secondary-contexts]} :data} :selected-context
                                     :as opts}]
  (get-aggregated-contexts db 
                           opts 
                           highlighted-secondary-contexts))

(defn search-issues 
  ;; prefer this signature
  ;; TODO  maybe selected-context the third param
  ([db q opts]
   (search-issues db (assoc opts :q q)))
  ([db opts]
   (search-issues' db opts)))

;; This is my preferred interface for searches where 
;; a context is actually selected
(defn _search-related-items 
  [_db 
   _q 
   selected-context-id 
   {:keys [_secondary-contexts-inverted
           _secondary-contexts-unassigned-selected
           _selected-secondary-contexts]
    :as _opts}
   {:keys [_limit _force-limit?]:as _ctx}]
  (when-not selected-context-id (throw (IllegalArgumentException. "selected-context-id must not be nil")))
  (let [_ {:id nil
           :data {:views {:current {}}}}]
    ;;
    ))
