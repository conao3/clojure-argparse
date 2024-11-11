(ns argparse.core
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log])
  (:gen-class))

(defn add-argument [parser option-string]
  (assoc parser :arguments [{:option-string option-string
                             :dest (keyword (subs option-string 1))}]))

(defn- numeric? [s]
  (some? (parse-long s)))

(defn- parse-option-arg [args option]
  (let [option-string (:option-string option)]
    (cond
      (empty? args) [nil args]

      (and (str/starts-with? (first args) option-string)
           (> (count (first args)) (count option-string)))
      [(subs (first args) (count option-string)) (rest args)]

      (= (first args) option-string)
      (if-let [value (second args)]
        (if (and (str/starts-with? value "-")
                 (not (numeric? value)))
          (throw (Exception. "Invalid argument format"))
          [value (drop 2 args)])
        (throw (Exception. "Missing argument for option")))

      :else [nil args])))

(defn parse-args [parser args]
  (let [option (first (:arguments parser))]
    (when (and (seq args)
               (not (str/starts-with? (first args) (:option-string option)))
               (not (str/starts-with? (first args) "-x")))
      (throw (Exception. "Invalid argument")))

    (if (empty? args)
      {(:dest option) nil}
      (let [[value remaining] (parse-option-arg args option)]
        (when (seq remaining)
          (throw (Exception. "Invalid additional arguments")))
        {(:dest option) value}))))

(defn -main
  "The entrypoint."
  [& args]
  (log/info args))
