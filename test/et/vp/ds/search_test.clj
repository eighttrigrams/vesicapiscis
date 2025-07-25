(ns et.vp.ds.search-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [et.vp.ds :as ds]
   [et.vp.ds.search :as search]
   [et.vp.ds.helpers :as helpers]
   [next.jdbc :as jdbc]
   [clojure.edn :as edn]))

(defonce db (edn/read-string (slurp "./test_config.edn")))

(defn reset-db []
  (jdbc/execute-one! db ["delete from relations"])
  (jdbc/execute-one! db ["delete from issues"]))

(def time-fn 
  (let [seconds (atom 0)]
    (fn [] 
      (let [current-seconds (mod @seconds 60)
            minutes (+ (quot @seconds 60) 0)
            time (str "'2025-01-01 10:" (format "%02d" minutes) ":" (format "%02d" current-seconds) "'")]
        (swap! seconds inc)
        time))))

(defmacro with-time [& body]
  `(with-redefs [helpers/gen-date time-fn
                 helpers/instant-now
                 (fn []
                   (java.time.Instant/parse "2025-01-02T05:45:00Z"))]
     ~@body))

(defmacro test-with-reset-db-and-time [string & body]
  `(testing ~string
     (reset-db)
     (with-time
       ~@body)))

(defn- related-items-q-all 
  [selected-context {:keys [q search-mode] :as opts}]
  (let [search-mode 
        (case search-mode
          :last-touched-first 1
          :past-events 4
          :last-added 5
          :integer-short-titles-asc 2
          :integer-short-titles-desc 3
          0)
        opts (dissoc (assoc opts :search-mode search-mode) :q)]
    (search/search-related-items db q (:id selected-context) opts {})))

(defn- related-items-q-titles [selected-context opts]
  (mapv :title (related-items-q-all selected-context opts)))

(defn- related-items-q
  "This fn is in place because I may want to do the refactoring where
   the search result isn't any longer just the first item of the vector
   but the only result."
  ([selected-context] (related-items-q selected-context {}))
  ([selected-context opts]
   (:title 
    (first
     (related-items-q-all selected-context opts)))))

(defn- items-q-all 
  ([q opts]
   (items-q-all q opts {}))
  ([q opts ctx]
   (search/search-items db q opts ctx)))

(defn- items-q-titles [q opts]
  (mapv :title (items-q-all q opts)))

(defn- items-q [q opts]
  (:title (first (items-q-all q opts))))

(defn- new-item 
  "In place because I want to end up having only new-item, 
   instead new-item and new-context"
  [db {:keys [title short-title sort-idx context-ids-set date]
       :or {short-title ""}
       :as opts}]
  (let [item
        (if context-ids-set
          (ds/new-item db title short-title context-ids-set sort-idx)
          (let [new-context (ds/new-context db opts)]
            (when short-title
              (ds/update-item db (assoc new-context :short_title short-title)))))]
    (if date
      (ds/update-item db (assoc item :date date))
      item)))

(defn- create-issues [{:keys [integer-short-titles?]}]
  (let [item-1    (new-item db {:title       "title-1" 
                                :short-title "abc"})
        item-2    (new-item db {:title       "title-2" 
                                :short-title "cde"})
        _item-1-1 (new-item db {:title           "title-1-1"
                                :short-title "abc"
                                :sort-idx 2
                                :context-ids-set #{(:id item-1)}
                                :date            "2025-01-03"})
        _item-1-2 (new-item db {:title           "title-1-2" 
                                :short-title "cde"
                                :sort-idx 1
                                :context-ids-set #{(:id item-1)}
                                :date            "2024-12-31"})
        _item-2-1 (new-item db {:title           "title-2-1"
                                :short-title     "abc"
                                :context-ids-set #{(:id (if-not integer-short-titles? item-2 item-1))}
                                :date            "2025-01-03"})
        _item-2-2 (new-item db {:title           "title-2-2" 
                                :short-title     "cde"
                                :context-ids-set #{(:id item-2)}
                                :date            "2025-01-04"})]
    [item-1 item-2]))

(deftest search
  (test-with-reset-db-and-time "base case - overview"
    (create-issues {})
    (is (= "title-2-2" (items-q "" {:all-items? true})))
    (is (= "title-2-1" (items-q "abc" {:all-items? true}))))
  (test-with-reset-db-and-time "in context"
    (let [[item-1 item-2] (create-issues {})]
      (is (= "title-1-2" (related-items-q item-1)))
      (is (= "title-1-1" (related-items-q item-1 {:search-mode :last-touched-first})))
      (is (= "title-1-1" (related-items-q item-1 {:q "abc"})))
      (is (= "title-2-2" (related-items-q item-2)))
      (is (= "title-2-1" (related-items-q item-2 {:search-mode :last-touched-first})))
      (is (= "title-2-1" (related-items-q item-2 {:q "abc"}))))))

(deftest events
  (test-with-reset-db-and-time 
   "base case - overview"
   (create-issues {})
   (is (= "title-2-2" (items-q "" {:all-items? true}))))
  (test-with-reset-db-and-time 
   "in context"
   (let [[_item-1 item-2] (create-issues {})]
     (is (= "title-2-2" (related-items-q item-2 {:search-mode :past-events})))))
  (test-with-reset-db-and-time 
   "last added mode"
   (let [[item-1 item-2] (create-issues {})]
     (is (= "title-2-2" (items-q "" {:search-mode :last-added :all-items? true})))
     (is (= "title-1-2" (related-items-q item-1 {:search-mode :last-added})))
     (is (= "title-2-2" (related-items-q item-2 {:search-mode :last-added}))))))

(deftest sort-modes
  (test-with-reset-db-and-time 
   "sort ascending and descending (only available in context)"
   (let [[item-1 _item-2] (create-issues {:integer-short-titles? true})]
     (is (= "title-1-2" (related-items-q item-1 {:search-mode :integer-short-titles-asc})))
     (is (= "title-1-1" (related-items-q item-1 {:search-mode :integer-short-titles-desc}))))))

(defn- create-issues-for-link-issue-test []
  (let [item-1    (new-item db {:title       "title-1"})
        item-2    (new-item db {:title       "title-2"})
        _item-3 (new-item db {:title           "title-3" 
                              :context-ids-set #{(:id item-1) 
                                                 (:id item-2)}})
        _item-4 (new-item db {:title           "title-4"
                              :context-ids-set #{(:id item-1)}})]
    [item-1 item-2]))

(deftest link-issue
  (test-with-reset-db-and-time
   "link-issue"
   (let [[item-1 item-2] (create-issues-for-link-issue-test)]
     (is (= ["title-2"] (items-q-titles "" {:link-issue true 
                                            :selected-context-id (:id item-1)
                                            :all-items? true})))
     (is (= ["title-4" "title-1"] (items-q-titles "" {:link-issue true
                                                      :selected-context-id (:id item-2)
                                                      :all-items? true}))))))

(defn- create-issues-for-intersection-tests [{add-one? :add-one?}]
  (let [item-1    (new-item db {:title       "title-1"})
        item-2    (new-item db {:title       "title-2"})
        item-5    (new-item db {:title       "title-5"})
        _item-3 (new-item db {:title           "title-3" 
                              :context-ids-set #{(:id item-1) 
                                                 (:id item-2)}})
        _item-4 (new-item db {:title           "title-4"
                              :context-ids-set #{(:id item-1)}})
        _ (when add-one? (new-item db {:title           "title-6"
                                       :context-ids-set #{(:id item-1) (:id item-5)}}))]
    ;; the test should work with and without this line -- TODO review/reenable
    #_(datastore.relations/set-the-containers-of-item! db 
                                           _item-3
                                           {(:id item-1) {:annotation "a"}
                                            (:id item-2) {:annotation nil}}
                                           false)
    [item-1 item-2]))

(deftest intersections
  (test-with-reset-db-and-time "base case - overview"
    (let [[item-1 item-2] (create-issues-for-intersection-tests {})]
      (is (= ["title-4" "title-3"] (related-items-q-titles item-1 {}))) ;; sanity check
      (is (= ["title-3"] (related-items-q-titles item-1 {:selected-secondary-contexts (list (:id item-2))})))
      (is (= ["title-4"] (related-items-q-titles item-1 {:secondary-contexts-unassigned-selected true})))
      (is (= ["title-3"] (related-items-q-titles item-1 {:selected-secondary-contexts (list (:id item-2))
                                            ;; when contexts list present, the following should have no effect
                                            :secondary-contexts-unassigned-selected true})))))
  (test-with-reset-db-and-time "base case - or"
    (let [[item-1 item-2] (create-issues-for-intersection-tests {})]
      (is (= ["title-4"] 
             (related-items-q-titles item-1 {:selected-secondary-contexts (list (:id item-2))
                               :secondary-contexts-inverted true})))
      (is (= []  
             (related-items-q-titles item-1 {:selected-secondary-contexts (list (:id item-2) (:id item-1))
                               :secondary-contexts-inverted true})))))
  (test-with-reset-db-and-time "base case - inverted and unassigned at the same time"
    (let [[item-1 _item-2] (create-issues-for-intersection-tests {})]
      (is (= ["title-3"]
             (related-items-q-titles item-1 {:secondary-contexts-inverted            true
                               :secondary-contexts-unassigned-selected true})))))
  (test-with-reset-db-and-time "base case - inverted and unassigned at the same time + secondary selected contexts"
     (let [[item-1 item-2] (create-issues-for-intersection-tests {})]
       (is (= []
              (related-items-q-titles item-1 {:secondary-contexts-inverted            true
                                :secondary-contexts-unassigned-selected true
                                :selected-secondary-contexts            (list (:id item-2))}))))
     (let [[item-1 item-2] (create-issues-for-intersection-tests {:add-one? true})]
       (is (= ["title-6"]
              (related-items-q-titles item-1 {:secondary-contexts-inverted            true
                                :secondary-contexts-unassigned-selected true
                                :selected-secondary-contexts            (list (:id item-2))}))))))

(defn- new-context 
  [db {:keys [short-title date]
       :or {short-title ""}
       :as opts}]
  (let [context (ds/new-context db opts)]
    (when (seq short-title)
      (ds/update-item db (assoc context :short_title short-title)))
    (if date
      (ds/update-item db (assoc context :date date))
      context)))

(defn- new-item'
  [db {:keys [title short-title context-ids-set sort-idx]
       :or {sort-idx 0}}]
  (ds/new-item db title short-title context-ids-set sort-idx))

(defn- create-contexts []
  (let [context-1 (new-context db {:title "Context One" :short-title "ctx1"})
        context-2 (new-context db {:title "Context Two" :short-title "ctx2"})  
        context-3 (new-context db {:title "Another Context" :short-title "ctx3"})
        ;; Create some issues to link to contexts
        _issue-1 (new-item' db {:title "Issue 1" 
                               :short-title "iss1"
                               :context-ids-set #{(:id context-1)}})
        _issue-2 (new-item' db {:title "Issue 2"
                               :short-title "iss2" 
                               :context-ids-set #{(:id context-2) (:id context-3)}})]
    [context-1 context-2 context-3]))

(deftest search-contexts-basic
  (test-with-reset-db-and-time "basic search-contexts functionality"
    (let [_contexts (create-contexts)
          all-contexts (items-q-all "" {})]
      ;; Should return all contexts
      (is (= 3 (count all-contexts)))
      (is (some #(= "Context One" (:title %)) all-contexts))
      (is (some #(= "Context Two" (:title %)) all-contexts))
      (is (some #(= "Another Context" (:title %)) all-contexts)))))

(deftest search-contexts-with-query
  (test-with-reset-db-and-time "search-contexts with search query"
    (let [_contexts (create-contexts)]
      ;; Search for "Context" should return contexts containing that word
      (let [results (items-q-all "Context" {})]
        ;; "Another Context" also contains "Context", so it should be included
        (is (= 3 (count results)))
        (is (some #(= "Context One" (:title %)) results))
        (is (some #(= "Context Two" (:title %)) results))
        (is (some #(= "Another Context" (:title %)) results)))
      
      ;; Search for "Another" should return only one context
      (let [results (items-q-all "Another" {})]
        (is (= 1 (count results)))
        (is (= "Another Context" (:title (first results)))))
      
      ;; Search for "One" should return only the context containing that word
      (let [results (items-q-all "One" {})]
        (is (= 1 (count results)))
        (is (= "Context One" (:title (first results)))))
      
      ;; Search for "Two" should return only the context containing that word  
      (let [results (items-q-all "Two" {})]
        (is (= 1 (count results)))
        (is (= "Context Two" (:title (first results))))))))

(deftest search-contexts-string-param
  (test-with-reset-db-and-time "search-contexts with string parameter"
    (let [_contexts (create-contexts)
          results (items-q-all "Context" {})]
      ;; Should handle string parameter by converting to opts map
      ;; "Context" should return all 3 contexts since they all contain "Context"
      (is (= 3 (count results)))
      (is (some #(= "Context One" (:title %)) results))
      (is (some #(= "Context Two" (:title %)) results))
      (is (some #(= "Another Context" (:title %)) results)))))

(deftest search-contexts-filtering
  (test-with-reset-db-and-time "search-contexts with filtering options"
    (let [[context-1 _context-2 _context-3] (create-contexts)
          results (items-q-all "" {:selected-context-id (:id context-1)})]
      ;; Should exclude the selected context from results
      (is (not (some #(= (:id context-1) (:id %)) results)))
      (is (some #(= "Context Two" (:title %)) results))
      (is (some #(= "Another Context" (:title %)) results)))))

(deftest search-contexts-limit
  (test-with-reset-db-and-time "search-contexts with limit"
    (let [_contexts (create-contexts)
          results (items-q-all "" {} {:limit 2})]
      ;; Should respect limit parameter
      (is (<= (count results) 2)))))

(deftest search-contexts-empty-query
  (test-with-reset-db-and-time "search-contexts with empty query"
    (let [_contexts (create-contexts)
          results (items-q-all "" {})]
      ;; Empty query should return all contexts
      (is (= 3 (count results))))))

(defn- create-contexts-for-link-test
  "Creates contexts with relationships for testing link-context functionality"
  []
  (let [context-1 (new-context db {:title "Main Context" :short-title "main"})
        context-2 (new-context db {:title "Related Context" :short-title "related"})
        context-3 (new-context db {:title "Other Context" :short-title "other"})
        ;; Create an issue with relationships to contexts via relations table
        issue-1 (new-item db {:title "Test Issue" 
                              :short-title "test"
                              :context-ids-set #{(:id context-1) (:id context-2)}})]
    [context-1 context-2 context-3 issue-1]))

(deftest search-contexts-link-context
  (test-with-reset-db-and-time "search-contexts with link-context"
    (let [[_context-1 _context-2 _context-3 issue-1] (create-contexts-for-link-test)
          ;; Test with selected-context that has issue relationships via relations table
          ;; context-1 and context-2 both contain the same issue, so context-2 should be excluded
          results-with-context (items-q-all
                                "" 
                                {:link-context true
                                 :selected-context-id (:id issue-1)})]
      ;; Should exclude context-1 (selected) and context-2 (shares issues with context-1)  
      ;; Should include context-3 (doesn't share issues with context-1)
      (is (= 1 (count results-with-context)))
      (is (= "Other Context" (:title (first results-with-context)))))))
