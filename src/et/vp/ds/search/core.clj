(ns et.vp.ds.search.core)

(def select [:issues.title
             :issues.short_title
             :issues.sort_idx
             :issues.id
             :issues.data
             [:issues.annotation :issue_annotation]
             :issues.is_context
             :issues.inserted_at
             :issues.updated_at
             :issues.date])

(defn exclusion-clause [selected-context-id mode]
  [:not [:in :issues.id 
         (if (= :issues mode)
           {:select :collections.item_id
            :from   :collections
            :where  [:= :collections.container_id [:inline selected-context-id]]}
           {:select :collections.container_id
            :from   :collections
            :where  [:= :collections.item_id selected-context-id]})]])
