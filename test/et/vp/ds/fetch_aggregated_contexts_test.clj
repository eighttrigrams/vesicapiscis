(ns et.vp.ds.fetch-aggregated-contexts-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [et.vp.ds :as ds]
   [et.vp.ds.search :as search]
   [et.vp.ds.helpers :as helpers]
   [next.jdbc :as jdbc]
   [clojure.edn :as edn]))

(defonce db (edn/read-string (slurp "./test_config.edn")))

(defn reset-db []
  (jdbc/execute-one! db ["delete from collections"])
  (jdbc/execute-one! db ["delete from issues"]))

(def time-fn 
  (let [seconds (atom 0)]
    (fn [] 
      (let [current-seconds (mod @seconds 60)
            minutes (+ (quot @seconds 60) 0)
            time (str "'2025-01-01 10:" (format "%02d" minutes) ":" (format "%02d" current-seconds) "'")]
        (swap! seconds inc)
        time))))

(defmacro with-time [& body]
  `(with-redefs [helpers/gen-date time-fn
                 helpers/instant-now
                 (fn []
                   (java.time.Instant/parse "2025-01-02T05:45:00Z"))]
     ~@body))

(defmacro test-with-reset-db-and-time [string & body]
  `(testing ~string
     (reset-db)
     (with-time
       ~@body)))

(defn- new-context 
  [db {:keys [short-title date]
       :or {short-title ""}
       :as opts}]
  (let [context (ds/new-context db opts)]
    (when (seq short-title)
      (ds/update-item db (assoc context :short_title short-title)))
    (if date
      (ds/update-item db (assoc context :date date))
      context)))

(deftest fetch-aggregated-contexts-prime-basic
  (test-with-reset-db-and-time "fetch-aggregated-contexts' basic functionality"
    (let [context-1 (new-context db {:title "Context 1" :short-title "ctx1"})
          context-2 (new-context db {:title "Context 2" :short-title "ctx2"})
          context-3 (new-context db {:title "Context 3" :short-title "ctx3"})
          issues [{:data {:contexts {(:id context-1) {:title "Context 1" :show-badge? true}
                                     (:id context-2) {:title "Context 2" :show-badge? true}}}}
                  {:data {:contexts {(:id context-1) {:title "Context 1" :show-badge? true}
                                     (:id context-3) {:title "Context 3" :show-badge? false}}}}
                  {:data {:contexts {(:id context-2) {:title "Context 2" :show-badge? true}}}}]
          result (search/fetch-aggregated-contexts' db issues [])]
      ;; Should aggregate contexts and count their occurrences
      ;; Context 1 appears 2 times (show-badge? true in both)
      ;; Context 2 appears 2 times (show-badge? true in both)
      ;; Context 3 appears 0 times (show-badge? false, so filtered out)
      (is (= 2 (count result)))
      (let [result-map (into {} result)]
        ;; Check that both contexts appear with count 2
        (is (= 2 (second (result-map (:id context-1)))))
        (is (= 2 (second (result-map (:id context-2)))))
        ;; Context 3 should not appear (show-badge? false)
        (is (nil? (result-map (:id context-3))))))))

(deftest fetch-aggregated-contexts-prime-empty-issues
  (test-with-reset-db-and-time "fetch-aggregated-contexts' with empty issues"
    (let [result (search/fetch-aggregated-contexts' db [] [])]
      ;; Should return empty result for empty issues
      (is (= 0 (count result))))))