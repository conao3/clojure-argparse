(ns argparse.core
  (:require
   [schema.core :as s]
   [clojure.tools.logging :as log]
   [argparse.util])
  (:gen-class))

(s/defschema Spec
  {:spec {s/Keyword s/Any}})

(s/defn ^:private find-flag :- (s/maybe
                                {:name s/Keyword
                                 :flag s/Str
                                 :spec {s/Keyword s/Any}})
  [spec :- Spec
   flag :- s/Str]
  (some #(when-let [flag (some (fn [x] (when (= flag x) x))
                               (argparse.util/ensure-vector (:flag (second %))))]
           {:name (first %)
            :flag flag
            :spec (second %)})
        (:spec spec)))

(s/defn ^:private parse-arg :- {:args {s/Keyword s/Any}
                                :rest [s/Str]}
  [spec :- Spec
   args :- [s/Str]]
  (argparse.util/cond-let
    [x (argparse.util/str-rest elm "--")] x
    [x (argparse.util/str-rest elm "-")] x))

(s/defn parse-args :- {:args {s/Keyword s/Any}
                       :rest [s/Str]}
  "Parse argument."
  [spec :- Spec
   args :- [s/Str]]
  (loop [args args
         ret {}]
    (if (empty? args)
      ret
      (let [elm (first args)]
        (argparse.util/cond-let
          [x (argparse.util/str-rest elm "--")] x
          [x (argparse.util/str-rest elm "-") x])
        (recur (rest args) ret*)))))

(defn -main
  "The entrypoint."
  [& args]
  (log/info args))
