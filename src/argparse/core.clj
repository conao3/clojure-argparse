(ns argparse.core
  (:require
   [schema.core :as s]
   [clojure.string :as str]
   [clojure.tools.logging :as log])
  (:gen-class))

(s/defschema Spec
  {:spec {s/Keyword s/Any}})

(s/defn eprintln :- (s/eq nil)
  "`println` but output to *err*."
  [& args :- [s/Any]]
  (binding [*out* *err*]
    (apply println args)))

(s/defn str-rest :- (s/maybe s/Str)
  "Check S is starts-with SUBSTR and then return rest of it."
  [s :- s/Str
   substr :- s/Str]
  (when (str/starts-with? s substr)
    (subs s (count substr))))

(defmacro cond-let
  [& clauses]
  (when clauses
    `(if-let ~(if (seq? (first clauses))
                (first clauses)
                `[_ ~(first clauses)])
       ~(second clauses)
       (cond-let ~@(nnext clauses)))))

(s/defn ensure-vector :- [s/Any]
  [arg :- s/Any]
  (if (vector? arg)
    arg
    [arg]))

(s/defn ^:private find-flag :- (s/maybe
                                {:name s/Keyword
                                 :flag s/Str
                                 :spec {s/Keyword s/Any}})
  [spec :- Spec
   flag :- s/Str]
  (some #(when-let [flag (some (fn [x] (when (= flag x) x))
                               (ensure-vector (:flag (second %))))]
           {:name (first %)
            :flag flag
            :spec (second %)})
        (:spec spec)))

(s/defn ^:private parse-arg :- {:args {s/Keyword s/Any}
                                :rest [s/Str]}
  [spec :- Spec
   args :- [s/Str]]
  (cond-let
    [x (str-rest elm "--")] x
    [x (str-rest elm "-")] x))

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
        (cond-let
          [x (str-rest elm "--")] x
          [x (str-rest elm "-") x])
        (recur (rest args) ret*)))))

(defn -main
  "The entrypoint."
  [& args]
  (log/info args))
