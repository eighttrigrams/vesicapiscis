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

(defn- create-issue []
  (let [item (ds/new-context db {:title "title1"})]
    (prn (:id item))))

(deftest search
  (testing "base case"
    (reset-db)
    (create-issue)
    (is (= "title1" (:title (ffirst (search/search-issues db {})))))))
