(ns et.vp.ds.search-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.edn :as edn]
            [next.jdbc :as jdbc]
            [et.vp.ds :as ds]
            [et.vp.ds.search :as search]
            [et.vp.ds.search.old :as search.old]
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
                 search.old/today-date (constantly "'2025-01-02'")
                 helpers/instant-now
                 (fn []
                   (java.time.Instant/parse "2025-01-02T05:45:00Z"))]
     ~@body))

(defn- q
  "This fn is in place because I may want to do the refactoring where
   the search result isn't any longer just the first item of the vector
   but the only result."
  ([selected-context] (q selected-context {}))
  ([selected-context {:keys [q]
                      :or {q ""}
                      :as opts}]
   (:title 
    (ffirst
     (search/search-issues 
      db 
      (if selected-context
        {:q q
         :selected-context 
         (assoc-in selected-context [:data :views :current] opts)}
        opts))))))

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

(defn- create-issues [{}]
  (let [item-1    (new-item db {:title       "title-1" 
                                :short-title "abc"})
        item-2    (new-item db {:title       "title-2" 
                                :short-title "cde"})
        _item-1-1 (new-item db {:title           "title-1-1"
                                :short-title     "abc"
                                :context-ids-set #{(:id item-1)}
                                :date            "2025-01-03"
                                :archived        false})
        _item-1-2 (new-item db {:title           "title-1-2" 
                                :short-title     "cde"
                                :context-ids-set #{(:id item-1)}
                                :date            "2025-01-04"
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
      (is (= "title-1-1" (q item-1 {:search-mode 1}))) ;; TODO name search modes
      (is (= "title-1-1" (q item-1 {:q "abc"})))
      (is (= "title-2-2" (q item-2)))
      (is (= "title-2-1" (q item-2 {:search-mode 1})))
      (is (= "title-2-1" (q item-2 {:q "abc"}))))))

(deftest events
  (test-with-reset-db-and-time 
   "base case - overview"
   (create-issues {:dates? true})
   (is (= "title-1-1" (q nil {:events-view 1}))))
  (test-with-reset-db-and-time 
   "base case - overview - arvhived events"
   (create-issues {:dates? true})
   (is (= "title-2-2" (q nil {:events-view 2}))));; TODO name events views
  (test-with-reset-db-and-time 
   "in context"
   (let [[item-1 item-2] (create-issues {:dates? true})]
     (is (= "title-1-1" (q item-1 {:events-view 1})))
     (is (= "title-2-2" (q item-2 {:events-view 2}))))))

;; TODO test pin events

;; TODO test intersections
