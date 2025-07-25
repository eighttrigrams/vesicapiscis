(ns et.vp.ds.ds-test
  (:require
   [clojure.test :refer [deftest is]]
   [et.vp.ds :as ds]
   [et.vp.ds.search-test :refer [test-with-reset-db-and-time db]]))

(deftest new-issue-test
  (test-with-reset-db-and-time "creates issue with database interaction"
    ;; Create a context first
    (let [context (ds/new-context db {:title "Test Context"})
          context-id (:id context)
          ;; Create issue using new-issue
          issue (ds/new-issue db 
                              "Test Issue Title" 
                              "short-title" 
                              #{context-id} 
                              1)]
      ;; Verify issue was created correctly
      (is (some? (:id issue)))
      (is (= "Test Issue Title" (:title issue)))
      (is (= "short-title" (:short_title issue)))
      (is (= 1 (:sort_idx issue)))
      ;; Verify database interaction - issue should be retrievable
      (let [retrieved-issue (ds/get-item db {:id (:id issue)})]
        (is (= (:title issue) (:title retrieved-issue)))
        (is (= (:short_title issue) (:short_title retrieved-issue)))
        (is (= (:sort_idx issue) (:sort_idx retrieved-issue))))))
  
  (test-with-reset-db-and-time "fails when no context provided"
    ;; Should throw exception when context-ids-set is empty
    (is (thrown? Exception 
                 (ds/new-issue db "Test" "test" #{} 0)))))



