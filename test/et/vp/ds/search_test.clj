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
                 search/today-date (constantly "'2025-01-02'")
                 helpers/instant-now
                 (fn []
                   (java.time.Instant/parse "2025-01-02T05:45:00Z"))]
     ~@body))

(defn- q
  "This fn is in place because I may want to do the refactoring where
   the search result isn't any longer just the first item of the vector
   but the only result."
  ([selected-context] (q selected-context {}))
  ([selected-context {:keys [q search-mode]
                      :or {q ""}
                      :as opts}]
   (let [[search-mode events-view] 
         (case search-mode
           :last-touched-first [1 0]
           :upcoming-events [0 1]
           :past-events [0 2]
           [0 0])
         opts (assoc opts 
                     :search-mode search-mode
                     :events-view events-view)]
     (:title 
      (ffirst
       (search/search-issues 
        db 
        (if selected-context
          {:q q
           :selected-context 
           (assoc-in selected-context [:data :views :current] opts)}
          opts)))))))

(defn- new-item 
  "In place because I want to end up having only new-item, 
   instead new-item and new-context"
  [db {:keys [title short-title context-ids-set date archived]
       :or {short-title ""}
       :as opts}]
  (let [item
        (if context-ids-set
          (ds/new-issue db title short-title context-ids-set)
          (let [new-context (ds/new-context db opts)]
            (when short-title
              (ds/update-item db (assoc new-context :short_title short-title)))))]
    (if date
      (ds/update-item db (assoc item :date date :archived archived))
      item)))

(defn- create-issues [{:keys [events-expired?]}]
  (let [item-1    (new-item db {:title       "title-1" 
                                :short-title "abc"})
        item-2    (new-item db {:title       "title-2" 
                                :short-title "cde"})
        _item-1-1 (new-item db {:title           "title-1-1"
                                :short-title     "abc"
                                :context-ids-set #{(:id item-1)}
                                :date            (if-not events-expired? 
                                                   "2025-01-03"
                                                   "2025-01-01")
                                :archived        false})
        _item-1-2 (new-item db {:title           "title-1-2" 
                                :short-title     "cde"
                                :context-ids-set #{(:id item-1)}
                                :date            (if-not events-expired? 
                                                   "2025-01-04"
                                                   "2024-12-31")
                                :archived        false})
        _item-2-1 (new-item db {:title           "title-2-1"
                                :short-title     "abc"
                                :context-ids-set #{(:id item-2)}
                                :date            "2025-01-03"
                                :archived        true})
        _item-2-2 (new-item db {:title           "title-2-2" 
                                :short-title     "cde"
                                :context-ids-set #{(:id item-2)}
                                :date            "2025-01-04" 
                                :archived        true})]
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
   (is (= "title-1-1" (q nil {:search-mode :upcoming-events}))))
  (test-with-reset-db-and-time 
   "base case - overview - arvhived events"
   (create-issues {:dates? true})
   (is (= "title-2-2" (q nil {:search-mode :past-events}))))
  (test-with-reset-db-and-time 
   "in context"
   (let [[item-1 item-2] (create-issues {})]
     (is (= "title-1-1" (q item-1 {:search-mode :upcoming-events})))
     (is (= "title-2-2" (q item-2 {:search-mode :past-events})))))
  (test-with-reset-db-and-time 
   "pin events"
   (let [[item-1] (create-issues {:events-expired? true})]
     (is (= "title-1-2" (q item-1 {}))))))

(defn- create-issues-for-intersection-tests [{}]
  (let [item-1    (new-item db {:title       "title-1"})
        item-2    (new-item db {:title       "title-2"})
        _item-3 (new-item db {:title           "title-3" 
                              :context-ids-set #{(:id item-1) 
                                                 (:id item-2)}})
        _item-4 (new-item db {:title           "title-4"
                              :context-ids-set #{(:id item-1)}})]
    [item-1 item-2]))

(deftest intersections
  (test-with-reset-db-and-time "base case - overview"
    (let [[item-1 item-2] (create-issues-for-intersection-tests {})]
      (is (= "title-4" (q item-1 {}))) ;; sanity check
      (is (= "title-3" (q item-1 {:selected-secondary-contexts (list (:id item-2))})))
      (is (= "title-4" (q item-1 {:selected-secondary-contexts (list (:id item-2))
                                  :secondary-contexts-inverted true})))
      ;; TODO fix this; this depends on data.contexts to be set properly
      #_(is (= "title-4" (q item-1 {:secondary-contexts-unassigned-selected true}))))))
