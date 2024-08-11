(ns argparse.util 
  (:require
    [schema.core :as s]
    [clojure.string :as str]))

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
