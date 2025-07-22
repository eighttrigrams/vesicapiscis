(ns et.vp.ds.search-contexts-test
  (:require
   [clojure.test :refer [deftest is]]
   [et.vp.ds :as ds]
   [et.vp.ds.search :as search]
   [et.vp.ds.search-test-helpers :refer [db test-with-reset-db-and-time]]))

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

(defn- new-issue
  [db {:keys [title short-title context-ids-set sort-idx]
       :or {sort-idx 0}}]
  (ds/new-issue db title short-title context-ids-set sort-idx))

(defn- create-contexts []
  (let [context-1 (new-context db {:title "Context One" :short-title "ctx1"})
        context-2 (new-context db {:title "Context Two" :short-title "ctx2"})  
        context-3 (new-context db {:title "Another Context" :short-title "ctx3"})
        ;; Create some issues to link to contexts
        _issue-1 (new-issue db {:title "Issue 1" 
                               :short-title "iss1"
                               :context-ids-set #{(:id context-1)}})
        _issue-2 (new-issue db {:title "Issue 2"
                               :short-title "iss2" 
                               :context-ids-set #{(:id context-2) (:id context-3)}})]
    [context-1 context-2 context-3]))

(deftest search-contexts-basic
  (test-with-reset-db-and-time "basic search-contexts functionality"
    (let [_contexts (create-contexts)
          all-contexts (search/search-contexts db {})]
      ;; Should return all contexts
      (is (= 3 (count all-contexts)))
      (is (some #(= "Context One" (:title %)) all-contexts))
      (is (some #(= "Context Two" (:title %)) all-contexts))
      (is (some #(= "Another Context" (:title %)) all-contexts)))))

(deftest search-contexts-with-query
  (test-with-reset-db-and-time "search-contexts with search query"
    (let [_contexts (create-contexts)]
      ;; Search for "Context" should return contexts containing that word
      (let [results (search/search-contexts db {:q "Context"})]
        ;; "Another Context" also contains "Context", so it should be included
        (is (= 3 (count results)))
        (is (some #(= "Context One" (:title %)) results))
        (is (some #(= "Context Two" (:title %)) results))
        (is (some #(= "Another Context" (:title %)) results)))
      
      ;; Search for "Another" should return only one context
      (let [results (search/search-contexts db {:q "Another"})]
        (is (= 1 (count results)))
        (is (= "Another Context" (:title (first results)))))
      
      ;; Search for "One" should return only the context containing that word
      (let [results (search/search-contexts db {:q "One"})]
        (is (= 1 (count results)))
        (is (= "Context One" (:title (first results)))))
      
      ;; Search for "Two" should return only the context containing that word  
      (let [results (search/search-contexts db {:q "Two"})]
        (is (= 1 (count results)))
        (is (= "Context Two" (:title (first results))))))))

(deftest search-contexts-string-param
  (test-with-reset-db-and-time "search-contexts with string parameter"
    (let [_contexts (create-contexts)
          results (search/search-contexts db "Context")]
      ;; Should handle string parameter by converting to opts map
      ;; "Context" should return all 3 contexts since they all contain "Context"
      (is (= 3 (count results)))
      (is (some #(= "Context One" (:title %)) results))
      (is (some #(= "Context Two" (:title %)) results))
      (is (some #(= "Another Context" (:title %)) results)))))

(deftest search-contexts-filtering
  (test-with-reset-db-and-time "search-contexts with filtering options"
    (let [[context-1 _context-2 _context-3] (create-contexts)
          results (search/search-contexts db {:selected-context context-1})]
      ;; Should exclude the selected context from results
      (is (not (some #(= (:id context-1) (:id %)) results)))
      (is (some #(= "Context Two" (:title %)) results))
      (is (some #(= "Another Context" (:title %)) results)))))

(deftest search-contexts-limit
  (test-with-reset-db-and-time "search-contexts with limit"
    (let [_contexts (create-contexts)
          results (search/search-contexts db {:limit 2})]
      ;; Should respect limit parameter
      (is (<= (count results) 2)))))

(deftest search-contexts-empty-query
  (test-with-reset-db-and-time "search-contexts with empty query"
    (let [_contexts (create-contexts)
          results (search/search-contexts db {:q ""})]
      ;; Empty query should return all contexts
      (is (= 3 (count results))))))