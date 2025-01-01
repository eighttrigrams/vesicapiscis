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

(def time-fn 
  (let [seconds (atom 0)]
    (fn [] 
      (let [time (str "'2025-01-01 10:00:" (format "%02d" @seconds) "'")]
        (swap! seconds inc)
        time))))

(defn with-time [body]
  (with-redefs [ds/gen-date time-fn]
    (body)))

(defn- q
  "This fn is in place because I may want to do the refactoring where
   the search result isn't any longer just the first item of the vector
   but the only result."
  ([] (q {}))
  ([opts]
   (first (search/search-issues db opts))))

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
  (let [item-1 (with-time
                 (fn []
                   (new-item db {:title "title-1"})))
        item-2 (with-time
                  (fn []
                    (new-item db {:title "title-2"})))
        _item-1-1 (with-time
                    (fn []
                      (new-item db {:title "title-1-1"
                                    :context-ids-set #{(:id item-1)}})))
        _item-1-2 (with-time
                    (fn []
                      (new-item db {:title "title-1-2" 
                                    :context-ids-set #{(:id item-1)}})))
        _item-2-1 (with-time
                    (fn []
                      (new-item db {:title "title-2-1"
                                    :context-ids-set #{(:id item-2)}})))
        _item-2-1 (with-time
                    (fn []
                      (new-item db {:title "title-2-2" 
                                    :context-ids-set #{(:id item-2)}})))]
    [item-1 item-2]))

(deftest search
  (testing "base case"
    (reset-db)
    (create-issue)
    (is (= "title-2-2" (:title (first (q))))))
  (testing "in context"
    (reset-db)
    (let [[item-1] (create-issue)]
      (is (= "title-1-2" (:title (first (q {:selected-context item-1}))))))))
