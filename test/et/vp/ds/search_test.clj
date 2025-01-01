(ns et.vp.ds.search-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.edn :as edn]
            [next.jdbc :as jdbc]
            [et.vp.ds :as ds]
            [et.vp.ds.search :as search]))

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

(defn with-time [body]
  (with-redefs [ds/gen-date time-fn]
    (body)))

(defn- q
  "This fn is in place because I may want to do the refactoring where
   the search result isn't any longer just the first item of the vector
   but the only result."
  ([] (q {}))
  ([opts]
   (:title (ffirst (search/search-issues db opts)))))

(defn- new-item 
  "In place because I want to end up having only new-item, 
   instead new-item and new-context"
  [db {:keys [title short-title context-ids-set]
       :or {short-title ""}
       :as opts}]
  (if context-ids-set
    (ds/new-issue db title short-title context-ids-set)
    (let [new-context (ds/new-context db opts)]
      (when short-title
        (ds/update-item db (assoc new-context :short_title short-title))))))

(defn- create-issue []
  (with-time
    (fn []
      (let [item-1 (new-item db {:title "title-1" :short-title "abc"})
            item-2 (new-item db {:title "title-2" :short-title "cde"})
            _item-1-1 (new-item db {:title           "title-1-1"
                                    :short-title     "abc"
                                    :context-ids-set #{(:id item-1)}})
            _item-1-2 (new-item db {:title           "title-1-2" 
                                    :short-title     "cde"
                                    :context-ids-set #{(:id item-1)}})
            _item-2-1 (new-item db {:title           "title-2-1"
                                    :short-title     "abc"
                                    :context-ids-set #{(:id item-2)}})
            _item-2-1 (new-item db {:title           "title-2-2" 
                                    :short-title     "cde"
                                    :context-ids-set #{(:id item-2)}})]
        [item-1 item-2]))))

;; TODO test pin events

(deftest search
  (testing "base case - overview"
    (reset-db)
    (create-issue)
    (is (= "title-2-2" (q)))
    (is (= "title-2-1" (q {:q "abc"}))))
  (testing "in context"
    (reset-db)
    (let [[item-1 item-2] (create-issue)]
      (is (= "title-1-2" (q {:selected-context item-1})))
      (is (= "title-1-1" (q {:selected-context (assoc-in item-1 [:data :views :current :search-mode] 1)})))
      (is (= "title-1-1" (q {:selected-context item-1
                              :q "abc"})))
      (is (= "title-2-2" (q {:selected-context item-2})))
      (is (= "title-2-1" (q {:selected-context (assoc-in item-2 [:data :views :current :search-mode] 1)})))
      (is (= "title-2-1" (q {:selected-context item-2
                              :q "abc"}))))))
