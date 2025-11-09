(ns hooks.cambium
  (:require [clj-kondo.hooks-api :as api]))

(defn deflevel [{:keys [:node]}]
  (let [[_ level-name] (:children node)]
    {:node (api/list-node
            (list
              (api/token-node 'defmacro)
              level-name
              (api/vector-node [(api/token-node '& )(api/token-node '_args)])
              (api/token-node 'nil)))}))
