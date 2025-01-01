(ns et.vp.ds.search-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.edn :as edn]
            [next.jdbc :as jdbc]
            [et.vp.ds :as ds]
            [et.vp.ds.search :as search]))

(defonce db (edn/read-string (slurp "./test_config.edn")))

(defn reset-db []
  (jdbc/execute-one! db ["delete from collections"])
  (jdbc/execute-one! db ["delete from issue_issue"])
  (jdbc/execute-one! db ["delete from issues"]))

(defn with-time [time body]
  (with-redefs [ds/gen-date (fn [] (str "'" time "'"))]
    (body)))

(defn- q
  "This fn is in place because I may want to do the refactoring where
   the search result isn't any longer just the first item of the vector
   but the only result."
  []
  (first (search/search-issues db {})))

(defn- new-item 
  "In place because I want to end up having only new-item, 
   instead new-item and new-context"
  [db {:keys [title short-title context-ids-set]
       :or {short-title ""}
       :as opts}]
  (if context-ids-set
    (ds/new-issue db title short-title context-ids-set)
    (ds/new-context db opts)))

(defn- create-issue []
  (let [item-1 (with-time "2025-01-01 10:00:00"
                  (fn []
                    (new-item db {:title "title-1"})))
        item-2 (with-time "2025-01-01 10:00:01" ;; TODO automate this, that every new item is inserted one second later in the tests
                  (fn []
                    (new-item db {:title "title-2"})))
        _item-1-1 (with-time "2025-01-01 10:00:02"
                  (fn []
                    (new-item db {:title "title-1-1"
                                   :context-ids-set #{(:id item-1)}})))
        _item-1-2 (with-time "2025-01-01 10:00:03"
                  (fn []
                    (new-item db {:title "title-1-2" 
                                   :context-ids-set #{(:id item-1)}})))
        _item-2-1 (with-time "2025-01-01 10:00:04"
                  (fn []
                    (new-item db {:title "title-2-1"
                                   :context-ids-set #{(:id item-2)}})))
        _item-2-1 (with-time "2025-01-01 10:00:05" 
                  (fn []
                    (new-item db {:title "title-2-2" 
                                   :context-ids-set #{(:id item-2)}})))]
    #_(prn (:id item-1))))

(deftest search
  (testing "base case"
    (reset-db)
    (create-issue)
    (is (= "title-2-2" (:title (first (q)))))))
