(ns et.vp.ds.helpers
  (:require [cheshire.core :as json]))

(defn namespace-keys
  [ns-str m]
  (into {} (map (fn [[k v]]
                  [(keyword (str ns-str "/" (name k))) v]) m)))

(defn un-namespace-keys
  [m]
  (into {}
        (map (fn [[k v]]
               [(keyword (name k)) v])
             m)))

(defn simplify-date [m]
  (update m :date 
          #(when % 
             (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") %))))

(defn- parse-data [context]
  (if (:data context)
    (update context :data #(json/parse-string (.toString %) true))
    context))

(defn post-process-base [query-result]
  (-> query-result
      un-namespace-keys
      simplify-date
      parse-data
      (dissoc :searchable)))
