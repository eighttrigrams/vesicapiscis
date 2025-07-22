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

#_(def contexts-select 
  [:issues.title
   :issues.short_title
   :issues.sort_idx
   :issues.id
   :issues.data
   :issues.is_context
   :issues.updated_at_ctx])