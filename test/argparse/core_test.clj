(ns argparse.core-test
  (:require
   [argparse.core :as sut]
   [clojure.test :as t]))

(t/deftest sample-test
  (t/is (= true true)))

(t/deftest find-if-test
  (t/is (= {:a 1}
           (sut/find-if #(find % :a) [{:a 1} {:b 2} {:c 3}])))
  (t/is (= {:b 2}
           (sut/find-if #(find % :b) [{:a 1} {:b 2} {:c 3}])))
  (t/is (= nil
           (sut/find-if #(find % :z) [{:a 1} {:b 2} {:c 3}]))))

(t/deftest test-parse-single-opt-test
  (let [parser (sut/add-argument {} "-x")]
    (t/testing "error"
      (t/is (thrown? Exception (sut/parse-single-opt parser "-x" [])))
      (t/is (thrown? Exception (sut/parse-single-opt parser "a" [])))
      (t/is (thrown? Exception (sut/parse-single-opt parser "-x" ["-y"]))))

    (t/testing "success"
      (t/is (= [:x "a" []]
               (sut/parse-single-opt parser "-x" ["a"])))

      ;; (t/is (= {:x "a"}
      ;;          (sut/parse-args parser ["-xa"])))
      
      (t/is (= [:x "-1" []]
               (sut/parse-single-opt parser "-x" ["-1"])))

      ;; (t/is (= {:x "-1"}
      ;;          (sut/parse-args parser ["-x-1"])))
      )))

(t/deftest test-optional-single-dash
  (let [parser (sut/add-argument {} "-x")]

    (t/testing "error"
      (t/is (thrown? Exception (sut/parse-args parser ["-x"])))
      (t/is (thrown? Exception (sut/parse-args parser ["a"])))
      (t/is (thrown? Exception (sut/parse-args parser ["--foo"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-x" "--foo"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-x" "-y"]))))

    (t/testing "success"
      (t/is (= {:x nil}
               (sut/parse-args parser [])))

      (t/is (= {:x "a"}
               (sut/parse-args parser ["-x" "a"])))

      ;; (t/is (= {:x "a"}
      ;;          (sut/parse-args parser ["-xa"])))

      (t/is (= {:x "-1"}
               (sut/parse-args parser ["-x" "-1"])))

      ;; (t/is (= {:x "-1"}
      ;;          (sut/parse-args parser ["-x-1"])))
      )))

(t/deftest test-optionals-single-dash-combined
  (let [parser (-> {}
                   (sut/add-argument "-x" :action :store-true)
                   (sut/add-argument "-yyy" :action :store-const :const 42)
                   (sut/add-argument "-z"))]

    (t/testing "failures"
      (t/is (thrown? Exception (sut/parse-args parser ["a"])))
      (t/is (thrown? Exception (sut/parse-args parser ["--foo"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-xa"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-x" "--foo"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-x" "-z"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-z" "-x"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-yx"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-yz" "a"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-yyyx"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-yyyz" "a"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-xyz" "a"])))
      (t/is (thrown? Exception (sut/parse-args parser ["-x="]))))

    (t/testing "successes"
      (t/is (= {:x false :yyy nil :z nil}
               (sut/parse-args parser [])))

      (t/is (= {:x true :yyy nil :z nil}
               (sut/parse-args parser ["-x"])))

      (t/is (= {:x false :yyy nil :z "a"}
               (sut/parse-args parser ["-za"])))

      (t/is (= {:x false :yyy nil :z "a"}
               (sut/parse-args parser ["-z" "a"])))

      (t/is (= {:x true :yyy nil :z "a"}
               (sut/parse-args parser ["-xza"])))

      (t/is (= {:x true :yyy nil :z "a"}
               (sut/parse-args parser ["-xz" "a"])))

      (t/is (= {:x true :yyy nil :z "a"}
               (sut/parse-args parser ["-x" "-za"])))

      (t/is (= {:x true :yyy nil :z "a"}
               (sut/parse-args parser ["-x" "-z" "a"])))

      (t/is (= {:x false :yyy 42 :z nil}
               (sut/parse-args parser ["-y"])))

      (t/is (= {:x false :yyy 42 :z nil}
               (sut/parse-args parser ["-yyy"])))

      (t/is (= {:x true :yyy 42 :z "a"}
               (sut/parse-args parser ["-x" "-yyy" "-za"])))

      (t/is (= {:x true :yyy 42 :z "a"}
               (sut/parse-args parser ["-x" "-yyy" "-z" "a"]))))))
