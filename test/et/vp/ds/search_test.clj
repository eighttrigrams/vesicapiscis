(ns et.vp.ds.search-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.edn :as edn]
            [next.jdbc :as jdbc]
            [et.vp.ds :as ds]
            [et.vp.ds.search :as search]
            [et.vp.ds.helpers :as helpers]))

(defonce db (edn/read-string (slurp "./test_config.edn")))

(defn reset-db []
  (jdbc/execute-one! db ["delete from collections"])
  (jdbc/execute-one! db ["delete from issues"]))

(def time-fn 
  (let [seconds (atom 0)]
    (fn [] 
      (let [time (str "'2025-01-01 10:00:" (format "%02d" @seconds) "'")]
        (swap! seconds inc)
        time))))

(defmacro with-time [& body]
  `(with-redefs [helpers/gen-date time-fn
                 helpers/instant-now
                 (fn []
                   (java.time.Instant/parse "2025-01-02T05:45:00Z"))]
     ~@body))

(defn- q-all 
  [selected-context {:keys [q search-mode]
                      :or {q ""}
                      :as opts}]
  (let [search-mode 
        (case search-mode
          :last-touched-first 1
          :past-events 4
           ;; 5 is missing, it is ""last added"
          :integer-short-titles-asc 2
          :integer-short-titles-desc 3
          0)
        opts (assoc opts :search-mode search-mode)]
    (first (search/search-issues 
            db 
            (if selected-context
              {:q q
               :selected-context 
               (assoc-in selected-context [:data :views :current] opts)}
              opts)))))

(defn- q-titles [selected-context opts]
  (mapv :title (q-all selected-context opts)))

(defn- q
  "This fn is in place because I may want to do the refactoring where
   the search result isn't any longer just the first item of the vector
   but the only result."
  ([selected-context] (q selected-context {}))
  ([selected-context opts]
   (:title 
    (first
     (q-all selected-context opts)))))

(defn- new-item 
  "In place because I want to end up having only new-item, 
   instead new-item and new-context"
  [db {:keys [title short-title context-ids-set date]
       :or {short-title ""}
       :as opts}]
  (let [item
        (if context-ids-set
          (ds/new-issue db title short-title context-ids-set {:suppress-digit-check? true})
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
                                :short-title     (if-not integer-short-titles? "abc" "2")
                                :context-ids-set #{(:id item-1)}
                                :date            "2025-01-03"})
        _item-1-2 (new-item db {:title           "title-1-2" 
                                :short-title     (if-not integer-short-titles? "cde" "1")
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

(defmacro test-with-reset-db-and-time [string & body]
  `(testing ~string
     (reset-db)
     (with-time
       ~@body)))

(deftest search
  (test-with-reset-db-and-time "base case - overview"
    (create-issues {})
    (is (= "title-2-2" (q nil)))
    (is (= "title-2-1" (q nil {:q "abc"}))))
  (test-with-reset-db-and-time "in context"
    (let [[item-1 item-2] (create-issues {})]
      (is (= "title-1-2" (q item-1)))
      (is (= "title-1-1" (q item-1 {:search-mode :last-touched-first})))
      (is (= "title-1-1" (q item-1 {:q "abc"})))
      (is (= "title-2-2" (q item-2)))
      (is (= "title-2-1" (q item-2 {:search-mode :last-touched-first})))
      (is (= "title-2-1" (q item-2 {:q "abc"}))))))

(deftest events
  (test-with-reset-db-and-time 
   "base case - overview"
   (create-issues {})
   (is (= "title-2-2" (q nil {}))))
  (test-with-reset-db-and-time 
   "in context"
   (let [[_item-1 item-2] (create-issues {})]
     (is (= "title-2-2" (q item-2 {:search-mode :past-events})))))
  ;; TODO add tests for "last added" (search mode 5)
  )

(deftest sort-modes
  (test-with-reset-db-and-time 
   "sort ascending and descending (only available in context)"
   #_(let [[item-1 _item-2] (create-issues {:integer-short-titles? true})]
     (is (= "title-1-2" (q item-1 {:search-mode :integer-short-titles-asc})))
     (is (= "title-1-1" (q item-1 {:search-mode :integer-short-titles-desc}))))))

(defn- create-issues-for-intersection-tests [{}]
  (let [item-1    (new-item db {:title       "title-1"})
        item-2    (new-item db {:title       "title-2"})
        item-3 (new-item db {:title           "title-3" 
                              :context-ids-set #{(:id item-1) 
                                                 (:id item-2)}})
        _item-4 (new-item db {:title           "title-4"
                              :context-ids-set #{(:id item-1)}})]
    ;; the test should work with and without this line -- TODO review/reenable
    #_(relations/set-the-containers-of-item! db 
                                           item-3
                                           {(:id item-1) {:annotation "a"}
                                            (:id item-2) {:annotation nil}}
                                           false)
    [item-1 item-2]))

(deftest intersections
  (test-with-reset-db-and-time "base case - overview"
    (let [[item-1 item-2] (create-issues-for-intersection-tests {})]
      (is (= ["title-4" "title-3"] (q-titles item-1 {}))) ;; sanity check
      (is (= ["title-3"] (q-titles item-1 {:selected-secondary-contexts (list (:id item-2))})))
      (is (= ["title-4"] (q-titles item-1 {:secondary-contexts-unassigned-selected true})))
      (is (= ["title-3"] (q-titles item-1 {:selected-secondary-contexts (list (:id item-2))
                                            ;; when contexts list present, the following should have no effect
                                            :secondary-contexts-unassigned-selected true})))))
  (test-with-reset-db-and-time "base case - or"
    (let [[item-1 item-2] (create-issues-for-intersection-tests {})]
      (is (= ["title-4"] 
             (q-titles item-1 {:selected-secondary-contexts (list (:id item-2))
                               :secondary-contexts-inverted true})))
      (is (= []  
             (q-titles item-1 {:selected-secondary-contexts (list (:id item-2) (:id item-1))
                               :secondary-contexts-inverted true})))))
  (test-with-reset-db-and-time "base case - inverted"
    (let [[item-1 item-2] (create-issues-for-intersection-tests {})]
      (is (= ;; TODO make these not sets but lists to also check for the correct ordering which still should hold 
           ["title-3"]
           (q-titles item-1 {:secondary-contexts-inverted true
                             :secondary-contexts-unassigned-selected true}))))))
