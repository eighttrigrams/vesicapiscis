(ns et.vp.ds.ds-test
  (:require
   [clojure.test :refer [deftest is]]
   [et.vp.ds :as ds]
   [et.vp.ds.relations :as relations]
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
  
  (test-with-reset-db-and-time "sets :is-context? property correctly for related items"
    ;; Create a context item (is_context = true)
    (let [context (ds/new-context db {:title "Test Context"})
          context-id (:id context)
          ;; Create a regular item (is_context = false) that will serve as a container
          regular-item (ds/new-item db "Regular Item" "regular" #{context-id} 1)
          regular-item-id (:id regular-item)
          ;; Create a new item that relates to both the context and the regular item
          test-item (ds/new-item db 
                                "Test Item with Mixed Containers" 
                                "mixed" 
                                #{context-id regular-item-id} 
                                2)]
      ;; Verify the context item has :is-context? set to true
      (is (= true (get-in test-item [:data :contexts context-id :is-context?])))
      ;; Verify the regular item has :is-context? set to false  
      (is (= false (get-in test-item [:data :contexts regular-item-id :is-context?])))))
  
  (test-with-reset-db-and-time "fails when no context provided"
    ;; Should throw exception when context-ids-set is empty
    (is (thrown? Exception 
                 (ds/new-item db "Test" "test" #{} 0)))))

(deftest link-item-to-another-item-test
  (test-with-reset-db-and-time "links item to another item with database interaction"
    ;; Create initial context and items
    (let [context (ds/new-context db {:title "Initial Context"})
          context-id (:id context)
          item1 (ds/new-item db "Item 1" "item1" #{context-id} 1)
          item2 (ds/new-item db "Item 2" "item2" #{context-id} 2)]
      ;; Link item1 to item2
      (relations/link-item-to-another-item! db item1 item2 true)
      ;; Retrieve updated item1 to verify the link
      (let [updated-item1 (ds/get-item db {:id (:id item1)})]
        ;; Verify item2 is now in item1's contexts
        (is (contains? (get-in updated-item1 [:data :contexts]) (:id item2)))
        (is (= "item2" (get-in updated-item1 [:data :contexts (:id item2) :title])))
        (is (= true (get-in updated-item1 [:data :contexts (:id item2) :show-badge?])))
        ;; Verify :is-context? is set correctly (item2 is not a context)
        (is (= false (get-in updated-item1 [:data :contexts (:id item2) :is-context?])))
        ;; Verify original context is still there
        (is (contains? (get-in updated-item1 [:data :contexts]) context-id))))))
  
  (test-with-reset-db-and-time "sets :is-context? property correctly when linking context and non-context items"
    ;; Create initial context and items
    (let [context (ds/new-context db {:title "Test Context"})
          context-id (:id context)
          item1 (ds/new-item db "Item 1" "item1" #{context-id} 1)
          another-context (ds/new-context db {:title "Another Context"})]
      ;; Link item1 to the context
      (relations/link-item-to-another-item! db item1 another-context true)
      ;; Link item1 to another regular item  
      (let [item2 (ds/new-item db "Item 2" "item2" #{context-id} 2)]
        (relations/link-item-to-another-item! db item1 item2 false)
        ;; Retrieve updated item1 to verify the links
        (let [updated-item1 (ds/get-item db {:id (:id item1)})]
          ;; Verify context has :is-context? = true
          (is (= true (get-in updated-item1 [:data :contexts (:id another-context) :is-context?])))
          ;; Verify regular item has :is-context? = false
          (is (= false (get-in updated-item1 [:data :contexts (:id item2) :is-context?])))
          ;; Verify show-badge? values are set correctly
          (is (= true (get-in updated-item1 [:data :contexts (:id another-context) :show-badge?])))
          (is (= false (get-in updated-item1 [:data :contexts (:id item2) :show-badge?])))))))

(deftest unlink-item-from-another-item-test
  (test-with-reset-db-and-time "unlinks item from another item successfully"
    ;; Create initial context and items
    (let [context (ds/new-context db {:title "Initial Context"})
          context-id (:id context)
          item1 (ds/new-item db "Item 1" "item1" #{context-id} 1)
          item2 (ds/new-item db "Item 2" "item2" #{context-id} 2)]
      ;; First link item1 to item2
      (relations/link-item-to-another-item! db item1 item2 true)
      ;; Verify the link was created
      (let [linked-item1 (ds/get-item db {:id (:id item1)})]
        (is (contains? (get-in linked-item1 [:data :contexts]) (:id item2))))
      ;; Now unlink item1 from item2
      (let [result (relations/unlink-item-from-another-item! db 
                                                            (ds/get-item db {:id (:id item1)}) 
                                                            item2)]
        ;; Verify unlink was successful
        (is (= true result))
        ;; Verify item2 is no longer in item1's contexts
        (let [updated-item1 (ds/get-item db {:id (:id item1)})]
          (is (not (contains? (get-in updated-item1 [:data :contexts]) (:id item2))))
          ;; Verify original context is still there
          (is (contains? (get-in updated-item1 [:data :contexts]) context-id)))))))
  
  (test-with-reset-db-and-time "fails to unlink when item would have no containers and is not a context"
    ;; Create a regular item with only one container
    (let [context (ds/new-context db {:title "Only Context"})
          context-id (:id context)
          item1 (ds/new-item db "Item 1" "item1" #{context-id} 1)]
      ;; Try to unlink from the only container
      (let [result (relations/unlink-item-from-another-item! db item1 context)]
        ;; Should fail because item would have no containers and is not a context
        (is (= false result))
        ;; Verify context is still there
        (let [unchanged-item1 (ds/get-item db {:id (:id item1)})]
          (is (contains? (get-in unchanged-item1 [:data :contexts]) context-id))))))
  
  (test-with-reset-db-and-time "allows unlinking when item is a context"
    ;; Create a context and make it a container for itself (edge case)
    (let [context1 (ds/new-context db {:title "Context 1"})
          context2 (ds/new-context db {:title "Context 2"})
          context1-id (:id context1)
          context2-id (:id context2)]
      ;; Link context1 to context2
      (relations/link-item-to-another-item! db context1 context2 true)
      ;; Verify the link was created
      (let [linked-context1 (ds/get-item db {:id context1-id})]
        (is (contains? (get-in linked-context1 [:data :contexts]) context2-id)))
      ;; Now unlink - should succeed because context1 is a context
      (let [result (relations/unlink-item-from-another-item! db 
                                                            (ds/get-item db {:id context1-id}) 
                                                            context2)]
        ;; Should succeed because context1 is a context (is_context = true)
        (is (= true result))
        ;; Verify context2 is no longer in context1's contexts
        (let [updated-context1 (ds/get-item db {:id context1-id})]
          (is (not (contains? (get-in updated-context1 [:data :contexts]) context2-id)))))))

(deftest switch-between-issue-and-context-test
  (test-with-reset-db-and-time "switches regular item to context and updates related items"
    ;; Create a regular item and another item that relates to it
    (let [context (ds/new-context db {:title "Initial Context"})
          context-id (:id context)
          item (ds/new-item db "Regular Item" "regular" #{context-id} 1)
          related-item (ds/new-item db "Related Item" "related" #{context-id} 2)]
      ;; Link the related item to the regular item
      (relations/link-item-to-another-item! db related-item item true)
      ;; Verify initial states
      (is (= false (:is_context item)))
      (let [initial-related (ds/get-item db {:id (:id related-item)})]
        ;; Verify the related item shows the regular item as :is-context? false
        (is (= false (get-in initial-related [:data :contexts (:id item) :is-context?]))))
      ;; Switch the regular item to a context
      (let [switched-item (ds/switch-between-issue-and-context! db item)]
        ;; Verify it's now a context
        (is (= true (:is_context switched-item)))
        ;; Verify the related item now shows it as :is-context? true
        (let [updated-related (ds/get-item db {:id (:id related-item)})]
          (is (= true (get-in updated-related [:data :contexts (:id item) :is-context?])))))))
  
  (test-with-reset-db-and-time "switches context to regular item when it has associated contexts and updates related items"
    ;; Create a context and items that relate to it
    (let [context1 (ds/new-context db {:title "Context 1"})
          context2 (ds/new-context db {:title "Context 2"})
          context1-id (:id context1)
          related-item (ds/new-item db "Related Item" "related" #{context1-id} 1)]
      ;; Link context1 to context2 (give context1 some associated contexts)
      (relations/link-item-to-another-item! db context1 context2 true)
      ;; Link the related item to context1
      (relations/link-item-to-another-item! db related-item context1 true)
      ;; Verify initial states
      (let [linked-context1 (ds/get-item db {:id context1-id})]
        (is (= true (:is_context linked-context1)))
        (is (seq (get-in linked-context1 [:data :contexts]))))
      (let [initial-related (ds/get-item db {:id (:id related-item)})]
        ;; Verify the related item shows context1 as :is-context? true
        (is (= true (get-in initial-related [:data :contexts context1-id :is-context?]))))
      ;; Switch context1 to a regular item
      (let [switched-item (ds/switch-between-issue-and-context! db (ds/get-item db {:id context1-id}))]
        ;; Verify it's now a regular item
        (is (= false (:is_context switched-item)))
        ;; Verify the related item now shows it as :is-context? false
        (let [updated-related (ds/get-item db {:id (:id related-item)})]
          (is (= false (get-in updated-related [:data :contexts context1-id :is-context?])))))))
  
  (test-with-reset-db-and-time "prevents switching context to regular item when it has no associated contexts"
    ;; Create a standalone context with no associated contexts
    (let [context (ds/new-context db {:title "Standalone Context"})]
      ;; Verify it's a context and has no associated contexts
      (is (= true (:is_context context)))
      (is (empty? (get-in context [:data :contexts] {})))
      ;; Try to switch it to a regular item - should fail silently
      (let [result (ds/switch-between-issue-and-context! db context)]
        ;; Should remain a context (no change)
        (is (= true (:is_context result)))
        ;; Verify database was not updated
        (let [retrieved-item (ds/get-item db {:id (:id context)})]
          (is (= true (:is_context retrieved-item)))))))
  
  (test-with-reset-db-and-time "updates multiple related items when switching context status"
    ;; Create one item and multiple items that relate to it
    (let [context (ds/new-context db {:title "Initial Context"})
          context-id (:id context)
          target-item (ds/new-item db "Target Item" "target" #{context-id} 1)
          related-item1 (ds/new-item db "Related Item 1" "rel1" #{context-id} 2)
          related-item2 (ds/new-item db "Related Item 2" "rel2" #{context-id} 3)
          related-item3 (ds/new-item db "Related Item 3" "rel3" #{context-id} 4)]
      ;; Link all related items to the target item
      (relations/link-item-to-another-item! db related-item1 target-item true)
      (relations/link-item-to-another-item! db related-item2 target-item true)
      (relations/link-item-to-another-item! db related-item3 target-item true)
      ;; Verify initial state - target item is not a context
      (is (= false (:is_context target-item)))
      ;; Switch target item to context
      (ds/switch-between-issue-and-context! db target-item)
      ;; Verify all related items now show target item as :is-context? true
      (let [updated-rel1 (ds/get-item db {:id (:id related-item1)})
            updated-rel2 (ds/get-item db {:id (:id related-item2)})
            updated-rel3 (ds/get-item db {:id (:id related-item3)})]
        (is (= true (get-in updated-rel1 [:data :contexts (:id target-item) :is-context?])))
        (is (= true (get-in updated-rel2 [:data :contexts (:id target-item) :is-context?])))
        (is (= true (get-in updated-rel3 [:data :contexts (:id target-item) :is-context?])))))))
