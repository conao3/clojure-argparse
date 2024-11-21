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

(m/=> take! [:function [:=> [:cat :any :any] :any]])
(defn take! [args n]
  (let [res (transient [])]
    (dotimes [_ n]
      (let [arg (first args)]
        (when (nil? arg)
          (throw (Exception. "Argument required.  Unexpected ending of input.")))
        (when (str/starts-with? arg "-")
          (throw (Exception. (str "Argument required.  Value is dash prefixed: " arg))))
        (conj! res arg)
        (swap! arg next)))
    (persistent! res)))

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
        (update :argument #(conj (vec %) arg))
        (assoc-in [:default dest] default))))

(m/=> parse-args [:function [:=> [:cat :any :any] :any]])
(defn parse-args
  ([parser args]
   (parse-args parser args (:default parser)))
  ([parser args res]
   (if (seq args)
     (let [args* (atom args)
           arg (first @args*)
           key (atom nil)
           parsed (atom nil)]
       (cond
         (str/starts-with? arg "-")
         (do
           (let [argument (or (find-if #((:option-string %) arg) (:argument parser))
                              (throw (Exception. (str "Invalid argument: " arg))))]
             (reset! key (:dest argument))
             (swap! args* next)
             (reset! parsed (first @args*))
             (when (and (str/starts-with? @parsed "-")
                        (not (numeric? @parsed)))
               (throw (Exception. (str "Argument is required: " arg))))
             (when (nil? @parsed)
               (throw (Exception. (str "Argument is required: " arg))))
             (swap! args* next)))

         :else
         (throw (Exception. "Not implemented")))
       (recur parser @args* (assoc res @key @parsed)))
     res)))

(defn -main
  "The entrypoint."
  [& args]
  (log/info args))
