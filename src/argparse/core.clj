(ns argparse.core
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [malli.core :as m])
  (:gen-class))

(m/=> numeric? [:function [:=> [:cat :string] :boolean]])
(defn numeric? [s]
  (or (some? (parse-long s))
      (some? (parse-double s))))

(m/=> find-if [:function [:=> [:cat :any [:any]] :any]])
(defn find-if [pred lst]
  (first (filter pred lst)))

(m/=> get-option-key [:function [:=> [:cat :string] :string]])
(defn get-option-key [arg]
  (when (or (= "-" arg)
            (= "--" arg))
    (throw (Exception. (str "option-string should not be - or --: " arg))))

  (or
   (when (str/starts-with? arg "---")
     (throw (Exception. (str "option-string should be prefixed - or --: " arg))))
   (when (str/starts-with? arg "--")
     (subs arg 2))
   (when (str/starts-with? arg "-")
     (subs arg 1))
   (throw (Exception. (str "option-string should be prefixed - or --: " arg)))))

(m/=> add-argument [:function [:=> [:cat :any [:or :string [:seqable :string]] :any] :string]])
(defn add-argument [parser option-string & {:keys [action dest default nargs]}]
  (let [option-string* (cond-> option-string
                         (string? option-string) vector)
        dest (keyword (or (some-> dest name)
                          (some-> (find-if #(str/starts-with? % "--") option-string*)
                                  (subs 2))
                          (some-> (find-if #(str/starts-with? % "-") option-string*)
                                  (subs 1))))
        arg (cond-> {:option-string (set option-string*)}
              dest (assoc :dest dest)
              action (assoc :action action)
              nargs (assoc :nargs nargs))]
    (-> parser
        (update :arguments #(conj (vec %) arg))
        (assoc-in [:default dest] default))))

(m/=> parse-single-opt [:function [:=>
                                   [:cat :any :string [:string]]
                                   [:tuple :keyword :any [:string]]]])
(defn parse-args [parser args]
  (let [res (atom (:default parser))
        args (atom args)
        position-args (atom [])]
    (loop []
      (let [arg (first @args)]
        (cond
          (str/starts-with? arg "-")
          (do
            (throw (Exception "not implemented"))
            (swap! args next))

          :else
          (do
            (swap! position-args conj arg)
            (swap! args next))))

      (when (seq @args)
        (recur)))
    @res))

(defn -main
  "The entrypoint."
  [& args]
  (log/info args))
