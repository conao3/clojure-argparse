(ns argparse.core
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [malli.core :as m])
  (:gen-class))

(defn add-argument [parser option-string & {:keys [action]}]
  (let [dest (keyword (subs option-string 1))
        arg {:option-string (subs option-string 1)
             :dest dest
             :action action}]
    (-> parser
        (update :arguments #(conj (vec %) arg))
        (assoc-in [:defaults dest] nil))))

(m/=> numeric? [:function [:=> [:cat :string] :boolean]])
(defn numeric? [s]
  (some? (parse-long s)))

(m/=> find-if [:function [:=> [:cat :any [:any]] :any]])
(defn find-if [pred lst]
  (first (filter pred lst)))

(m/=> parse-single-opt [:function [:=>
                                   [:cat :any :string [:string]]
                                   [:tuple :keyword :any [:string]]]])
(defn parse-single-opt [parser arg args]
  (let [remaining (atom args)
        argument (or (find-if #(= (subs arg 1) (:option-string %)) (:arguments parser))
                     (throw (Exception. (str "Invalid argument: " arg))))
        parsed (if-let [f (:action argument)]
                 (f)
                 (let [target-arg (first args)]
                   (when (nil? target-arg)
                     (throw (Exception. (str "Required argument for: " arg))))

                   ;; accept -x-1 but -x-a is not accepted
                   ;; short option like -1 is not permitted, so -1 shoule be argument of -x.
                   (when (and (str/starts-with? target-arg "-")
                         (not (numeric? target-arg)))
                     (throw (Exception. (str "Required argument for: " arg))))

                   (swap! remaining rest)
                   target-arg))]

    [(:dest argument) parsed @remaining]))

(defn parse-args [parser args]
  (loop [result (:defaults parser {})
         remaining args]
    (if (empty? remaining)
      result
      (let [arg (first remaining)
            [dest parsed next-args]
            (cond
              (str/starts-with? arg "--")
              (throw (Exception. (str "Invalid argument: " arg))) ; wip

              (str/starts-with? arg "-")
              (parse-single-opt parser arg (rest remaining))

              :else (throw (Exception. (str "Invalid argument: " arg))))]
        (recur (assoc result dest parsed) next-args)))))

(defn -main
  "The entrypoint."
  [& args]
  (log/info args))
