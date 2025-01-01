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

(defn- create-issue []
  (let [_item-1 (with-time "2025-01-01 10:00:00"
                  (fn []
                    (ds/new-context db {:title "title-1"})))
        _item-2 (with-time "2025-01-01 10:00:01" ;; TODO automate this, that every new item is inserted one second later in the tests
                  (fn []
                    (ds/new-context db {:title "title-2"})))]
    #_(prn (:id item-1))))

(defn- q
  "This fn is in place because I may want to do the refactoring where
   the search result isn't any longer just the first item of the vector
   but the only result."
  []
  (first (search/search-issues db {})))

(deftest search
  (testing "base case"
    (reset-db)
    (create-issue)
    (is (= "title-2" (:title (first (q)))))))
