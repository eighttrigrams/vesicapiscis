(ns et.vp.ds.search-items-test
  (:require
   [clojure.test :refer [deftest is]]
   [et.vp.ds :as ds]
   [et.vp.ds.search :as search]
   [et.vp.ds.search-test-helpers :refer [db test-with-reset-db-and-time]]))

(defn- search-items
  "Wrapper around search/search-items for potential future implementation changes"
  [db opts]
  (search/search-items db opts))

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

(defn- new-item
  [db {:keys [title short-title context-ids-set sort-idx]
       :or {sort-idx 0}}]
  (ds/new-issue db title short-title context-ids-set sort-idx))

(defn- create-contexts []
  (let [context-1 (new-context db {:title "Context One" :short-title "ctx1"})
        context-2 (new-context db {:title "Context Two" :short-title "ctx2"})  
        context-3 (new-context db {:title "Another Context" :short-title "ctx3"})
        ;; Create some issues to link to contexts
        _issue-1 (new-item db {:title "Issue 1" 
                               :short-title "iss1"
                               :context-ids-set #{(:id context-1)}})
        _issue-2 (new-item db {:title "Issue 2"
                               :short-title "iss2" 
                               :context-ids-set #{(:id context-2) (:id context-3)}})]
    [context-1 context-2 context-3]))

(deftest search-contexts-basic
  (test-with-reset-db-and-time "basic search-contexts functionality"
    (let [_contexts (create-contexts)
          all-contexts (search-items db {})]
      ;; Should return all contexts
      (is (= 3 (count all-contexts)))
      (is (some #(= "Context One" (:title %)) all-contexts))
      (is (some #(= "Context Two" (:title %)) all-contexts))
      (is (some #(= "Another Context" (:title %)) all-contexts)))))

(deftest search-contexts-with-query
  (test-with-reset-db-and-time "search-contexts with search query"
    (let [_contexts (create-contexts)]
      ;; Search for "Context" should return contexts containing that word
      (let [results (search-items db {:q "Context"})]
        ;; "Another Context" also contains "Context", so it should be included
        (is (= 3 (count results)))
        (is (some #(= "Context One" (:title %)) results))
        (is (some #(= "Context Two" (:title %)) results))
        (is (some #(= "Another Context" (:title %)) results)))
      
      ;; Search for "Another" should return only one context
      (let [results (search-items db {:q "Another"})]
        (is (= 1 (count results)))
        (is (= "Another Context" (:title (first results)))))
      
      ;; Search for "One" should return only the context containing that word
      (let [results (search-items db {:q "One"})]
        (is (= 1 (count results)))
        (is (= "Context One" (:title (first results)))))
      
      ;; Search for "Two" should return only the context containing that word  
      (let [results (search-items db {:q "Two"})]
        (is (= 1 (count results)))
        (is (= "Context Two" (:title (first results))))))))

(deftest search-contexts-string-param
  (test-with-reset-db-and-time "search-contexts with string parameter"
    (let [_contexts (create-contexts)
          results (search-items db "Context")]
      ;; Should handle string parameter by converting to opts map
      ;; "Context" should return all 3 contexts since they all contain "Context"
      (is (= 3 (count results)))
      (is (some #(= "Context One" (:title %)) results))
      (is (some #(= "Context Two" (:title %)) results))
      (is (some #(= "Another Context" (:title %)) results)))))

(deftest search-contexts-filtering
  (test-with-reset-db-and-time "search-contexts with filtering options"
    (let [[context-1 _context-2 _context-3] (create-contexts)
          results (search-items db {:selected-context context-1})]
      ;; Should exclude the selected context from results
      (is (not (some #(= (:id context-1) (:id %)) results)))
      (is (some #(= "Context Two" (:title %)) results))
      (is (some #(= "Another Context" (:title %)) results)))))

(deftest search-contexts-limit
  (test-with-reset-db-and-time "search-contexts with limit"
    (let [_contexts (create-contexts)
          results (search-items db {:limit 2})]
      ;; Should respect limit parameter
      (is (<= (count results) 2)))))

(deftest search-contexts-empty-query
  (test-with-reset-db-and-time "search-contexts with empty query"
    (let [_contexts (create-contexts)
          results (search-items db {:q ""})]
      ;; Empty query should return all contexts
      (is (= 3 (count results))))))

(defn- create-contexts-for-link-test
  "Creates contexts with relationships for testing link-context functionality"
  []
  (let [context-1 (new-context db {:title "Main Context" :short-title "main"})
        context-2 (new-context db {:title "Related Context" :short-title "related"})
        context-3 (new-context db {:title "Other Context" :short-title "other"})
        ;; Create an issue with relationships to contexts via collections table
        issue-1 (new-item db {:title "Test Issue" 
                              :short-title "test"
                              :context-ids-set #{(:id context-1) (:id context-2)}})]
    [context-1 context-2 context-3 issue-1]))

(deftest search-contexts-link-context
  (test-with-reset-db-and-time "search-contexts with link-context"
    (let [[context-1 _context-2 _context-3 issue-1] (create-contexts-for-link-test)
          ;; Test with selected-context that has issue relationships via collections table
          ;; context-1 and context-2 both contain the same issue, so context-2 should be excluded
          results-with-context (search-items db {:link-context true
                                                    :selected-context issue-1})]
      ;; Should exclude context-1 (selected) and context-2 (shares issues with context-1)  
      ;; Should include context-3 (doesn't share issues with context-1)
      (is (= 1 (count results-with-context)))
      (is (= "Other Context" (:title (first results-with-context)))))))