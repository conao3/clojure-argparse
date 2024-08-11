(ns argparse.core-test
  (:require
   [argparse.core :as argparse]
   [clojure.test :as t]))

(t/deftest sample-test
  (t/is (= true true)))

(defmacro ^:redef parser-test-case
  {:style/indent 1}
  [spec & {:keys [failures successes]}]
  (let [[& {:as successes*}] successes]
    `(do
       ~@(map (fn [elm]
                `(let [{:keys [rest#]} (argparse/parse-args ~spec ~elm)]
                   (t/is (seq? rest#))))
              failures)
       ~@(map (fn [elm]
                `(let [{:keys [args# rest#]} (argparse/parse-args ~spec ~(first elm))]
                   (t/is (empty? rest#))
                   (t/is (= args# ~(second elm)))))
              successes*))))

(t/deftest find-flag-test
  (t/is (= {:name :x
            :flag "-x"
            :spec {:flag "-x"}}
           (let [spec {:spec {:x {:flag "-x"}
                              :y {:flag "-y"}}}]
             (#'argparse/find-flag spec "-x"))))

  (t/is (= {:name :y
            :flag "-y"
            :spec {:flag "-y"}}
           (let [spec {:spec {:x {:flag "-x"}
                              :y {:flag "-y"}}}]
             (#'argparse/find-flag spec "-y"))))

  (t/is (nil?
         (let [spec {:spec {:x {:flag "-x"}
                            :y {:flag "-y"}}}]
           (#'argparse/find-flag spec "-z"))))

  (t/is (= {:name :x
            :flag "-x"
            :spec {:flag ["-x" "--execute"]}}
           (let [spec {:spec {:x {:flag ["-x" "--execute"]}
                              :y {:flag "-y"}}}]
             (#'argparse/find-flag spec "-x")))))

(t/deftest test-optionals-single-dash
  (parser-test-case {:spec {:x {:flag "-x"}}}
    :failures ["-x" "a" "--foo" "-x --foo" "-x -y"]
    :successes ["" {:x nil}
                "-x a" {:x "a"}
                "-xa" {:x "a"}
                "-x -1" {:x "-1"}
                "-x-1" {:x "-1"}]))
