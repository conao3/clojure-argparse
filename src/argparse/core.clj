(ns argparse.core
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log])
  (:gen-class))

(defn add-argument [parser option-string & {:keys [action const]}]
  (let [dest (keyword (subs option-string 1))
        arg {:option-string option-string
             :dest dest
             :action action
             :const const}]
    (-> parser
        (update :arguments #(conj (vec %) arg))
        (assoc-in [:defaults dest] (if (= action :store-true) false nil)))))

(defn- numeric? [s]
  (some? (parse-long s)))

(defn- find-matching-arg [parser opt-string]
  (some #(when (or (= (:option-string %) opt-string)
                   (str/starts-with? opt-string (:option-string %))) %)
        (:arguments parser)))

(defn- handle-combined-format [arg-def opt-str]
  (let [opt-len (count (:option-string arg-def))]
    (when (> (count opt-str) opt-len)
      (subs opt-str opt-len))))

(defn- parse-single-opt [parser opt-string args]
  (if-let [arg (find-matching-arg parser opt-string)]
    (case (:action arg)
      :store-true  [{(:dest arg) true} args]
      :store-const [{(:dest arg) (:const arg)} args]
      (let [combined (handle-combined-format arg opt-string)
            value (if combined combined (first args))]
        (if (and value (str/starts-with? value "-") (not (numeric? value)))
          (throw (Exception. (str "Invalid argument: " value)))
          [{(:dest arg) value} (if combined args (rest args))])))
    (throw (Exception. (str "Invalid option: " opt-string)))))

(defn parse-args [parser args]
  (loop [remaining args
         result (:defaults parser {})]
    (if (empty? remaining)
      result
      (let [arg (first remaining)
            [parsed next-args]
            (if (str/starts-with? arg "-")
              (parse-single-opt parser arg (rest remaining))
              (throw (Exception. (str "Invalid argument: " arg))))]
        (recur next-args (merge result parsed))))))

(defn -main
  "The entrypoint."
  [& args]
  (log/info args))
