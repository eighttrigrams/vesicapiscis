(ns et.vp.ds.search
  (:require [clojure.set :as set]
            [cambium.core :as log]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [et.vp.ds.search.new :as search.new]
            [et.vp.ds.helpers
             :refer [un-namespace-keys post-process-base]
             :as helpers]
            [et.vp.ds.search.helpers :as search.helpers]))

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

(defn update-contexts [item]
  (update-in item [:data :contexts] 
             (fn [contexts]
               (into {} 
                     (map (fn [[k v]]
                            [(Integer/parseInt (name k)) (if (map? v) v
                                                             {:title       v
                                                              :show-badge? true})])
                          contexts)))))

(defn post-process [query-result]
  (-> query-result
      post-process-base
      update-contexts))

(defn- query-string-contexts-query [q _selected-context]
  (sql/format (merge {:select [:issues.title
                               :issues.short_title
                               :issues.short_title_ints
                               :issues.id
                               :issues.data
                               :issues.is_context
                               :issues.updated_at_ctx]
                      :from  [:issues]
                      :where [:and
                              (when-not (= "" (or q ""))
                                [:raw (format "searchable @@ to_tsquery('simple', '%s')"
                                              (search.helpers/convert-q-to-query-string q))])
                              [:= :issues.is_context true]]
                      :order-by [[:updated_at_ctx :desc]]}
                     (when true #_(and (not selected-context)
                                (= "" (or q "")))
                       {:limit 100}))))

(defn- filter-contexts [{:keys [link-context selected-context selected-issue]} contexts]
  (if-not link-context
    (remove #(= (:id selected-context) (:id %)) contexts)
    (let [ids-of-contexts-to-remove (conj (set (map #(if (number? %)
                                                       %
                                                       (Integer/parseInt (if (keyword? %) 
                                                                           (name %) 
                                                                           %))) 
                                                    (keys (or (:contexts (:data selected-issue))
                                                              (:contexts (:data selected-context))))))
                                          (:id (or selected-issue selected-context)))]
      (remove #(ids-of-contexts-to-remove (:id %)) contexts))))

(defn search-contexts
  [db opts]
  (let [opts (if (string? opts) 
               {:q opts}
               opts)
        {:keys [q]} opts]
    (try
      (->>
       (query-string-contexts-query q (:selected-context opts))
       (jdbc/execute! db)
       (map post-process)
       (filter-contexts opts))
      (catch Exception e
        (log/error (str "error in search/search-contexts: " e " - param was: " q))
        (throw e)))))

(defn- filter-by-selected-secondary-contexts'
  [{:keys [selected-secondary-contexts
           secondary-contexts-unassigned-selected]}
   issues]
  (let [selected-secondary-contexts-set (into #{} selected-secondary-contexts)]
    (remove
     (fn [issue]
       (or
        (and secondary-contexts-unassigned-selected
             (= 1 (count (:contexts (:data issue))))) 
        (seq (set/intersection 
              (set (keys (:contexts (:data issue))))
              selected-secondary-contexts-set))))
     issues)))

(defn- no-modifiers-selected? [{:keys [secondary-contexts-unassigned-selected
                                       secondary-contexts-inverted]}]
  (not (or secondary-contexts-inverted 
           secondary-contexts-unassigned-selected)))

(defn- filter-by-selected-secondary-contexts 
  [{:keys [link-issue? selected-context]}
   issues]
  (let [{:keys [secondary-contexts-inverted]
         :as current-view} 
          (-> selected-context :data :views :current)]
    (if (and (not link-issue?)
             (not (no-modifiers-selected? current-view))
             secondary-contexts-inverted)
      (filter-by-selected-secondary-contexts' current-view issues)
      issues)))

(defn- do-query [db formatted-query]
  #_(prn "???" formatted-query)
  (let [issues (jdbc/execute! db formatted-query)]
    (log/info (str "count: " (count issues)))
    issues))

(defn- join-ids [selected-context]
  (let [current-view                (-> selected-context :data :views :current)
        selected-secondary-contexts (-> current-view :selected-secondary-contexts)] 
    (when (and (seq selected-secondary-contexts)  
               (or (no-modifiers-selected? current-view)
                   (and (:secondary-contexts-inverted current-view)
                        (not (:secondary-contexts-unassigned-selected current-view)))))
      selected-secondary-contexts)))

(defn- or-mode? [selected-context]
  (let [current-view (-> selected-context :data :views :current)]
    (and (:secondary-contexts-inverted current-view) 
         (not (:secondary-contexts-unassigned-selected current-view)))))

(defn- do-fetch-ids 
  [db {:keys [selected-context link-issue?]
       :as   state} search-mode]
  (let [selected-context-id (:id selected-context)
        issues (do-query db 
                         (search.new/fetch-issues 
                          (or (:q state) "") 
                          {:selected-context-id (when-not link-issue? selected-context-id)
                           :force-limit?        link-issue?
                           :search-mode         search-mode
                           :unassigned-mode?    (:secondary-contexts-unassigned-selected (-> selected-context :data :views :current))
                           :inverted-mode?      (:secondary-contexts-inverted (-> selected-context :data :views :current))
                           :join-ids            (when-not link-issue? 
                                                  (join-ids selected-context))
                           :or-mode? (or-mode? selected-context)}
                          {:limit 500}))]
    (seq issues)))

;; TODO this can be filtered out in new.clj; write test for this
(defn- filter-issues-already-related-to-current-context
  [{:keys [link-issue?
           selected-context]} issues]
  (if link-issue?
    (remove #(or ((set (keys (:contexts (:data %)))) (:id selected-context))
                (= (:id %) (:id selected-context))) issues)
    issues))

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
  (let [opts (assoc opts 
                    :link-issue? (= :context (:link-issue opts))
                    :link-issue nil)
        opts (update opts :selected-context (partial modify opts))]
    (->> (do-fetch-ids db opts search-mode)
         (map post-process)
         (filter-by-selected-secondary-contexts opts)
         (filter-issues-already-related-to-current-context opts))))

(defn- try-parse [item]
  (try (Integer/parseInt item)
       (catch Exception _e nil)))

(defn- pre-process-highlighted-secondary-contexts
  [highlighted-secondary-contexts]
  (->> highlighted-secondary-contexts
       (map try-parse)
       (remove nil?)))

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
  (let [opts (
                ;; TODO instead of doing this, make sure q is always at least ""
              if (:q opts) 
               (update opts :q search.helpers/remove-some-chars)
                 ;; for destructuring in searcj-issues' to work properly when :q is present but has nil value
               (dissoc opts :q))]
    (get-aggregated-contexts db 
                             opts 
                             highlighted-secondary-contexts)))

(defn search-issues [db opts]
  (let [opts (
                ;; TODO instead of doing this, make sure q is always at least ""
              if (:q opts) 
               (update opts :q search.helpers/remove-some-chars)
                 ;; for destructuring in searcj-issues' to work properly when :q is present but has nil value
               (dissoc opts :q))]
    [(search-issues' db opts) {}]))
