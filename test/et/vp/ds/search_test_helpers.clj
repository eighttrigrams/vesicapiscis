(ns et.vp.ds.search-test-helpers
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [testing]]
   [et.vp.ds.helpers :as helpers]
   [next.jdbc :as jdbc]))

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