(ns personalist.datastore.search
  (:require [clojure.set :as set]
            [cambium.core :as log]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [personalist.datastore.helpers
             :refer [un-namespace-keys post-process-base]]))

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

(def select [:issues.title
             :issues.short_title
             :issues.short_title_ints
             :issues.id
             :issues.data
             :issues.is_context
             :issues.updated_at
             :issues.date
             :issues.archived])

(defn remove-some-chars [q]
  (-> q
      (str/replace "(" " ")
      (str/replace ")" " ")
      (str/replace "[" " ")
      (str/replace "]" " ")
      (str/replace "|" " ")
      (str/replace "!" " ")
      (str/replace "&" " ")
      (str/replace "'" " ")
      (str/replace ":" " ")
      (str/replace "{" " ")
      (str/replace "}" " ")
      (str/replace "  " " ")
      (str/replace "  " " ")
      (str/trim)))

(defn convert-q-to-query-string [q]
  (let [qs
        (str/join " & " (map #(str % ":*") (str/split (remove-some-chars q) #" ")))]
    (if (= ":*" qs)
      "*"
      qs)))

(defn- wrap-order-and-limit [formatted-query selected-context link-issue]
  (let [formatted-query (if (and selected-context link-issue) 
                          (let [[q :as original-query] formatted-query
                                formatted-query        (str "SELECT * FROM (" q ") AS issues ORDER BY issues.updated_at DESC LIMIT 500")]
                            (assoc original-query 0 formatted-query))
                          formatted-query)]
    #_(log/info (str "formatted-query: " formatted-query))
    formatted-query))

(defn- get-search-clause [q]
  (when (not= "" q)
    [:raw (format "searchable @@ to_tsquery('simple', '%s')" 
                  (convert-q-to-query-string q))]))

(defn- get-events-exist-clause [events-view]
  (when (not= 0 events-view)
    [:and
     [:<> :issues.date nil]
     [:not= :issues.archived [:inline (= 1 events-view)]]]))

(defn do-fetch-ids'' 
  [{:keys [q link-issue]
    :or   {q ""}} 
   selected-context
   search-mode
   events-view
   issue-ids-to-remove
   join-ids
   and-query?]
  (-> 
   (sql/format 
    (merge
     {:select (if selected-context (vec (concat select [:collections.annotation]))
                  select)
      :from   [:issues]
      :where  [:and [:and
                     (get-events-exist-clause events-view)
                     (when join-ids [:in :collections.container_id [:inline join-ids]])
                     (get-search-clause q)]
               (when issue-ids-to-remove
                 [:not [:in :issues.id [:inline issue-ids-to-remove]]])]}
     (when join-ids
       {:group-by (if selected-context
                    [:issues.id :collections.annotation]
                    [:issues.id])
        :join     [:collections [:= :issues.id :collections.item_id]]})
     (when and-query?
       {:having [:raw (str "COUNT(issues.id) = " (count join-ids))]})
     (when-not (and selected-context link-issue)
       {:order-by [[:issues.updated_at (if (= 1 search-mode)
                                         :asc 
                                         :desc)]]})
     (when (and (= "" q)
                (not selected-context)
                (= 0 events-view))
       {:limit 500})))
    ;; TODO i could do the sorting and limiting uniformly here
   (wrap-order-and-limit selected-context link-issue)))

(defn do-fetch-ids' 
  "This should later be a replacement for do-fetch-ids"
  [{:keys [q _link-issue]
    :or   {q ""}} 
   _selected-context
   _search-mode
   _events-view
   _issue-ids-to-remove
   _join-ids
   _and-query?]
  )

(defmacro sectime
  [what expr]
  `(let [start# (. System (currentTimeMillis))
         ret# ~expr]
     #_(log/info (str "Elapsed time - " ~what ": " (/ (double (- (. System (currentTimeMillis)) start#)) 1000.0) " secs"))
     ret#))

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
                                              (convert-q-to-query-string q))])
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

(defn- re-order [search-mode issues]
  (if (= 2 search-mode)
    (sort-by #(:short_title_ints %)
             (filter #(> (:short_title_ints %) 0) issues))
    (reverse (sort-by #(:short_title_ints %)
                                (filter #(> (:short_title_ints %) 0) issues)))))

(defn- expired-filter [issue]
  (and
   (not (:archived issue))
   (:date issue)
   (= 1 (.compareTo (java.time.Instant/now)
                    (java.time.Instant/parse (str (:date issue) "T05:45:00Z"))))))

;; TODO extract common pattern (top,bottom) with #re-order
(defn- pin-events [issues]
  (let [top (filter expired-filter issues)
        bottom (remove  expired-filter issues)]
    (concat top bottom)))

;; TODO most of this, when not inverted and not unassigned selected, can be done as part of the query in do-fetch-ids' 
(defn- filter-by-selected-secondary-contexts 
  [{:keys [link-issue]
    {{{{:keys [selected-secondary-contexts
              secondary-contexts-unassigned-selected
              secondary-contexts-inverted]} :current} :views} :data} :selected-context}
   issues]
  (let [selected-secondary-contexts-set (into #{} selected-secondary-contexts)
        link-issue? link-issue]
    (if (and (not link-issue?)
             (or secondary-contexts-unassigned-selected
                 secondary-contexts-inverted))
      ((if-not secondary-contexts-inverted filter remove)
       (fn [issue]
         (or 
          (and secondary-contexts-unassigned-selected
               (= 1 (count (:contexts (:data issue)))))
          
          (if-not secondary-contexts-inverted
            (and (not secondary-contexts-unassigned-selected)
                 (every? identity (map #(contains? (set (keys (:contexts (:data issue)))) %) 
                                       selected-secondary-contexts-set)))
            (seq (set/intersection 
                  (set (keys (:contexts (:data issue))))
                  selected-secondary-contexts-set)))))
       issues)
      issues)))

(defn- get-events-view 
  [{{{{{:keys [events-view]} :current} :views} :data
     :as                                       selected-context} :selected-context
    global-events-view :events-view}]
  (or (if selected-context
        events-view
        global-events-view) 0))

(defn- do-fetch-urgent-issues-ids [db link-issue events-view selected-context q]
  (let [urgent-events-query
        (sql/format
         {:select   select
          :from     [:issues]
          :order-by [[:updated_at :desc]]
          :where    [:and
                     [:<> :issues.date nil]
                     [:not= :issues.archived true]
                     [:< 
                      :date
                      [:raw "NOW()"]]]})]
    (if (or link-issue 
            (not= 0 events-view)
            selected-context
            (and (string? q) (not= "" q)))
          '()
          (jdbc/execute! 
           db 
           urgent-events-query))))

(defn- do-query [db formatted-query]
  (let [issues (jdbc/execute! db formatted-query)]
    #_(log/info (str "count: " (count issues)))
    issues))

(defn- do-fetch-ids 
  [db {:keys [q search-globally? selected-context selected-issue link-issue]
       :or   {q ""}
       :as   state} search-mode]
  (let [context-ids-to-join-on-link-issue-context (-> selected-context :data :views :current :selected-secondary-contexts)
        context-ids-to-join-on-link-issue-issue (keys (:contexts (:data selected-issue)))
        selected-context (when (:id selected-context) selected-context)
        originally-selected-context selected-context
        selected-context (if (and search-globally?
                                  (not 
                                   (and (= :context link-issue)
                                        (seq context-ids-to-join-on-link-issue-context)))) 
                           nil selected-context)
        events-view (get-events-view state)
        urgent-issues-ids (do-fetch-urgent-issues-ids db link-issue events-view selected-context q)
        urgent-issues-ids-simplified (when (seq urgent-issues-ids) (map :issues/id urgent-issues-ids))
        secondary-contexts-but-no-modifiers-selected? (let [{{{{:keys [selected-secondary-contexts
                                            secondary-contexts-inverted
                                            secondary-contexts-unassigned-selected]} :current} :views} :data} selected-context]
                             (not (or secondary-contexts-inverted
                                      secondary-contexts-unassigned-selected
                                      (not (seq selected-secondary-contexts)))))
        join-ids (if selected-context
                   (if link-issue
                     (if (= :issue link-issue)
                       context-ids-to-join-on-link-issue-issue
                       context-ids-to-join-on-link-issue-context)
                     (if secondary-contexts-but-no-modifiers-selected?
                       (conj context-ids-to-join-on-link-issue-context
                             (:id selected-context))
                       [(:id selected-context)]))
                   (when (and (not search-globally?) 
                              (= :issue link-issue)
                              (seq context-ids-to-join-on-link-issue-issue)
                              (not originally-selected-context))
                     context-ids-to-join-on-link-issue-issue)) 
        and-query? (or (and selected-context (= :context link-issue)) 
                       secondary-contexts-but-no-modifiers-selected?)
        issues-ids (do-query db 
                             (do-fetch-ids'' state 
                                             selected-context 
                                             search-mode 
                                             events-view 
                                             urgent-issues-ids-simplified 
                                             join-ids
                                             and-query?))]
    (seq (concat urgent-issues-ids issues-ids))))

(defn- filter-issues
  [{:keys [link-issue 
           selected-issue
           selected-context]} issues]
  (if-not link-issue 
    (remove #(= (:id selected-issue) (:id %)) issues)
    (if (= :issue link-issue)
      (let [issue-ids-to-exclude (conj (set (map :id (:related_issues selected-issue)))
                                       (:id selected-issue))]
        (remove #(issue-ids-to-exclude (:id %)) 
                issues))
      (remove #(or ((set (keys (:contexts (:data %)))) (:id selected-context))
                   (= (:id %) (:id selected-context))) issues))))

(defn- sort-for-events-view 
  [issues events-view]
  (->> issues 
       (sort-by :date)
       (filter :date)
       (#(if (= 2 events-view)
           (reverse %)
           %))))

(defn- sort-for-regular-view 
  [issues 
   {{{{{:keys [search-mode]} :current} :views} :data} :selected-context
    :keys [link-issue]}]
  (cond->> issues
    (#{2 3} search-mode)
    (re-order search-mode)
    (and (not link-issue)
         (not (#{2 3} search-mode))) 
    pin-events))

(defn- sort-issues [state 
                    issues]
  (let [events-view (get-events-view state)
        in-events-view? (not= 0 events-view)]
    (if in-events-view? 
      (sort-for-events-view issues events-view)
      (sort-for-regular-view issues state))))

(defn- search-issues'
  [db {{{{{:keys [search-mode]} :current} :views} :data} :selected-context
       :as opts}]
       (let [issues (do-fetch-ids db opts search-mode)]
         (->> issues
              (map post-process)
              (sort-issues opts)
              (filter-by-selected-secondary-contexts opts)
              (filter-issues opts))))

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
                                      (assoc :events-view 0)
                                      (assoc-in [:selected-context :data :views :current :events-view] 0)
                                      (assoc-in [:selected-context :data :views :current :search-mode] 0)
                                      (assoc-in [:selected-context :data :views :current :selected-secondary-contexts] [])
                                      (assoc-in [:selected-context :data :views :current :secondary-contexts-inverted] false)
                                      (assoc-in [:selected-context :data :views :current :secondary-contexts-unassigned-selected] false)))]
    (sectime
     "get-aggregated-contexts#after-search-issues'"
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

          (sort-secondary-contexts db highlighted-secondary-contexts)))))

(defn fetch-aggregated-contexts [db {{{:keys [highlighted-secondary-contexts]} :data} :selected-context
                                     :as opts}]
  #_(log/info (str "fetch-aggregated-contects " (:title (:selected-context opts))))
  (sectime
   "fetch-aggregated-contexts"
   (let [opts (
                ;; TODO instead of doing this, make sure q is always at least ""
               if (:q opts) 
                (update opts :q remove-some-chars)
                 ;; for destructuring in searcj-issues' to work properly when :q is present but has nil value
                (dissoc opts :q))]
     (sectime "get-aggregated-contexts"
              (get-aggregated-contexts db 
                                       opts 
                                       highlighted-secondary-contexts)))))

(defn search-issues [db {:keys [skip-context-aggregation?
                                only-context-aggregation?]
                         :as opts}]
  ;; (log/info (str "search-issues - skip-context-aggregation? " skip-context-aggregation? " : only-context-aggregation? " only-context-aggregation?))
  (sectime
   "search-issues"
   (let [opts (
                ;; TODO instead of doing this, make sure q is always at least ""
               if (:q opts) 
                (update opts :q remove-some-chars)
                 ;; for destructuring in searcj-issues' to work properly when :q is present but has nil value
                (dissoc opts :q))]
     [(sectime "search-issues/issues" 
               (search-issues' db opts)) {}])))
