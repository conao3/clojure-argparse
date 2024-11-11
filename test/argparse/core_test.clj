(ns argparse.core-test
  (:require
   [argparse.core :as sut]
   [clojure.test :as t]))

(t/deftest sample-test
  (t/is (= true true)))

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

      (t/is (= {:x "a"}
               (sut/parse-args parser ["-xa"])))

      (t/is (= {:x "-1"}
               (sut/parse-args parser ["-x" "-1"])))

      (t/is (= {:x "-1"}
               (sut/parse-args parser ["-x-1"]))))))
