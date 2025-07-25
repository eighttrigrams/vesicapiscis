(ns et.vp.ds.ds-test
  (:require
   [clojure.test :refer [deftest is]]
   [et.vp.ds :as ds]
   [et.vp.ds.search-test :refer [test-with-reset-db-and-time db]]))

(deftest new-item-test
  (test-with-reset-db-and-time "creates item with database interaction"
    ;; Create a context first
    (let [context (ds/new-context db {:title "Test Context"})
          context-id (:id context)
          ;; Create item using new-item
          item (ds/new-item db 
                            "Test Item Title" 
                            "short-title" 
                            #{context-id} 
                            1)]
      ;; Verify item was created correctly
      (is (some? (:id item)))
      (is (= "Test Item Title" (:title item)))
      (is (= "short-title" (:short_title item)))
      (is (= 1 (:sort_idx item)))
      ;; Verify database interaction - item should be retrievable
      (let [retrieved-item (ds/get-item db {:id (:id item)})]
        (is (= (:title item) (:title retrieved-item)))
        (is (= (:short_title item) (:short_title retrieved-item)))
        (is (= (:sort_idx item) (:sort_idx retrieved-item))))
      ;; Verify collection titles are set (effect of set-collection-titles-of-new-issue call)
      ;; The item should have context information populated in its data
      (is (map? (get-in item [:data :contexts])))
      (is (contains? (get-in item [:data :contexts]) context-id))
      (is (= "Test Context" (get-in item [:data :contexts context-id :title])))))
  
  (test-with-reset-db-and-time "fails when no context provided"
    ;; Should throw exception when context-ids-set is empty
    (is (thrown? Exception 
                 (ds/new-item db "Test" "test" #{} 0)))))



