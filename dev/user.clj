(ns user
  (:require
   [schema.core :as s]))

(s/set-fn-validation! true)
(alter-var-root #'*warn-on-reflection* (constantly true))

(defn hello []
  "world")

(defn sample1 []
  (let [parser {:name "ProgramName"
                :description "What the program does"
                :epilog "Text at the bottom of help"}])
  )
