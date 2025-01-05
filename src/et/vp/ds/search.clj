(ns et.vp.ds.search
  (:require [et.vp.ds.search.old :as old-search]))

(defn fetch-aggregated-contexts [db opts]
  (old-search/fetch-aggregated-contexts db opts))

;; TODO desired interface db selected-context search-opts
(defn search-issues [db opts]
  (old-search/search-issues db opts))

(defn search-contexts [db opts] (old-search/search-contexts db opts))