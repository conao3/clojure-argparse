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

;; (t/deftest test-parse-single-opt-test
;;   (let [parser (-> {}
;;                    (sut/add-argument "-x"))]
;;     (t/testing "error"
;;       (t/is (thrown? Exception (sut/parse-single-opt parser "-x" [])))
;;       (t/is (thrown? Exception (sut/parse-single-opt parser "a" [])))
;;       (t/is (thrown? Exception (sut/parse-single-opt parser "-x" ["-y"]))))

;;     (t/testing "success"
;;       (t/is (= [:x "a" []]
;;                (sut/parse-single-opt parser "-x" ["a"])))

;;       ;; (t/is (= {:x "a"}
;;       ;;          (sut/parse-args parser ["-xa"])))

;;       (t/is (= [:x "-1" []]
;;                (sut/parse-single-opt parser "-x" ["-1"])))

;;       ;; (t/is (= {:x "-1"}
;;       ;;          (sut/parse-args parser ["-x-1"])))
;;       )))

(t/deftest test-optional-single-dash
  (let [parser (-> {}
                   (sut/add-argument "-x"))]

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

;; (t/deftest test-optionals-single-dash-combined
;;   (let [parser (-> {}
;;                    (sut/add-argument "-x" :action (constantly true))
;;                    (sut/add-argument "-yyy" :action (constantly 42))
;;                    (sut/add-argument "-z"))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--foo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-xa"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-x" "--foo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-x" "-z"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-z" "-x"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-yx"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-yz" "a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-yyyx"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-yyyz" "a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-xyz" "a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-x="]))))

;;     (t/testing "successes"
;;       ;; WARN: :action store_true => :action (constantly true)
;;       ;; WARN: :action store_const :const 42 => :action (constantly 42)
;;       ;; WARN: While store_true infers the argument type as
;;       ;;       boolean and sets the default value to `false', in this
;;       ;;       implementation it doesn't do anything special so the
;;       ;;       default value is `nil'.

;;       (t/is (= {:x nil :yyy nil :z nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:x true :yyy nil :z nil}
;;                (sut/parse-args parser ["-x"])))

;;       ;; (t/is (= {:x nil :yyy nil :z "a"}
;;       ;;          (sut/parse-args parser ["-za"])))

;;       (t/is (= {:x nil :yyy nil :z "a"}
;;                (sut/parse-args parser ["-z" "a"])))

;;       ;; (t/is (= {:x true :yyy nil :z "a"}
;;       ;;          (sut/parse-args parser ["-xza"])))

;;       ;; (t/is (= {:x true :yyy nil :z "a"}
;;       ;;          (sut/parse-args parser ["-xz" "a"])))

;;       ;; (t/is (= {:x true :yyy nil :z "a"}
;;       ;;          (sut/parse-args parser ["-x" "-za"])))

;;       (t/is (= {:x true :yyy nil :z "a"}
;;                (sut/parse-args parser ["-x" "-z" "a"])))

;;       ;; why?
;;       ;; (t/is (= {:x nil :yyy 42 :z nil}
;;       ;;          (sut/parse-args parser ["-y"])))

;;       (t/is (= {:x nil :yyy 42 :z nil}
;;                (sut/parse-args parser ["-yyy"])))

;;       ;; (t/is (= {:x true :yyy 42 :z "a"}
;;       ;;          (sut/parse-args parser ["-x" "-yyy" "-za"])))

;;       (t/is (= {:x true :yyy 42 :z "a"}
;;                (sut/parse-args parser ["-x" "-yyy" "-z" "a"]))))))

;; (t/deftest test-optionals-single-dash-long
;;   (let [parser (-> {}
;;                    (sut/add-argument "-foo"))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--foo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foo" "--foo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foo" "-y"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-fooa"]))))

;;     (t/testing "successes"
;;       (t/is (= {:foo nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:foo "a"}
;;                (sut/parse-args parser ["-foo" "a"])))

;;       (t/is (= {:foo "-1"}
;;                (sut/parse-args parser ["-foo" "-1"])))

;;       ;; (t/is (= {:foo "a"}
;;       ;;          (sut/parse-args parser ["-fo" "a"])))

;;       ;; (t/is (= {:foo "a"}
;;       ;;          (sut/parse-args parser ["-f" "a"])))
;;       )))

;; (t/deftest test-optionals-single-dash-subset-ambiguous
;;   (let [parser (-> {}
;;                    (sut/add-argument "-f")
;;                    (sut/add-argument "-foobar")
;;                    (sut/add-argument "-foorab"))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["-f"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-fo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foo" "b"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foob"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-fooba"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foora"]))))

;;     (t/testing "successes"
;;       (t/is (= {:f nil :foobar nil :foorab nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:f "a" :foobar nil :foorab nil}
;;                (sut/parse-args parser ["-f" "a"])))

;;       ;; (t/is (= {:f "a" :foobar nil :foorab nil}
;;       ;;          (sut/parse-args parser ["-fa"])))

;;       ;; (t/is (= {:f "oa" :foobar nil :foorab nil}
;;       ;;          (sut/parse-args parser ["-foa"])))

;;       ;; (t/is (= {:f "ooa" :foobar nil :foorab nil}
;;       ;;          (sut/parse-args parser ["-fooa"])))

;;       (t/is (= {:f nil :foobar "a" :foorab nil}
;;                (sut/parse-args parser ["-foobar" "a"])))

;;       (t/is (= {:f nil :foobar nil :foorab "a"}
;;                (sut/parse-args parser ["-foorab" "a"]))))))

;; (t/deftest test-optionals-single-dash-ambiguous
;;   (let [parser (-> {}
;;                    (sut/add-argument "-foobar")
;;                    (sut/add-argument "-foorab"))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["-f"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-f" "a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-fa"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foa"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-fo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foo" "b"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-f=a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-foo=b"]))))

;;     (t/testing "successes"
;;       (t/is (= {:foobar nil :foorab nil}
;;                (sut/parse-args parser [])))

;;       ;; (t/is (= {:foobar "a" :foorab nil}
;;       ;;          (sut/parse-args parser ["-foob" "a"])))

;;       ;; (t/is (= {:foobar "a" :foorab nil}
;;       ;;          (sut/parse-args parser ["-foob=a"])))

;;       ;; (t/is (= {:foobar nil :foorab "a"}
;;       ;;          (sut/parse-args parser ["-foor" "a"])))

;;       ;; (t/is (= {:foobar nil :foorab "a"}
;;       ;;          (sut/parse-args parser ["-foor=a"])))

;;       ;; (t/is (= {:foobar "a" :foorab nil}
;;       ;;          (sut/parse-args parser ["-fooba" "a"])))

;;       ;; (t/is (= {:foobar "a" :foorab nil}
;;       ;;          (sut/parse-args parser ["-fooba=a"])))

;;       ;; (t/is (= {:foobar nil :foorab "a"}
;;       ;;          (sut/parse-args parser ["-foora" "a"])))

;;       ;; (t/is (= {:foobar nil :foorab "a"}
;;       ;;          (sut/parse-args parser ["-foora=a"])))

;;       (t/is (= {:foobar "a" :foorab nil}
;;                (sut/parse-args parser ["-foobar" "a"])))

;;       (t/is (= {:foobar "a" :foorab nil}
;;                (sut/parse-args parser ["-foobar=a"])))

;;       (t/is (= {:foobar nil :foorab "a"}
;;                (sut/parse-args parser ["-foorab" "a"])))

;;       (t/is (= {:foobar nil :foorab "a"}
;;                (sut/parse-args parser ["-foorab=a"]))))))

;; (t/deftest test-optionals-numeric
;;   (let [parser (-> {}
;;                    (sut/add-argument "-1" :dest :one))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["-1"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-1" "--foo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-1" "-y"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-1" "-1"])))
;;       ;; (t/is (thrown? Exception (sut/parse-args parser ["-1" "-2"])))  ; why don't we do this, `-x -1` is accepted
;;       )

;;     (t/testing "successes"
;;       (t/is (= {:one nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:one "a"}
;;                (sut/parse-args parser ["-1" "a"])))

;;       ;; (t/is (= {:one "a"}
;;       ;;          (sut/parse-args parser ["-1a"])))

;;       ;; (t/is (= {:one "-2"}
;;       ;;          (sut/parse-args parser ["-1-2"])))
;;       )))

;; (t/deftest test-optionals-double-dash
;;   (let [parser (-> {}
;;                    (sut/add-argument "--foo"))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["--foo"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-f"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-f" "a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--foo" "-x"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--foo" "--bar"]))))

;;     (t/testing "successes"
;;       (t/is (= {:foo nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:foo "a"}
;;                (sut/parse-args parser ["--foo" "a"])))

;;       (t/is (= {:foo "a"}
;;                (sut/parse-args parser ["--foo=a"])))

;;       (t/is (= {:foo "-2.5"}
;;                (sut/parse-args parser ["--foo" "-2.5"])))

;;       (t/is (= {:foo "-2.5"}
;;                (sut/parse-args parser ["--foo=-2.5"]))))))

;; (t/deftest test-optionals-double-dash-partial-match
;;   (let [parser (-> {}
;;                    (sut/add-argument "--badger" :action (constantly true))
;;                    (sut/add-argument "--bat"))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["--bar"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--b"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--ba"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--b=2"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--ba=4"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--badge" "5"]))))

;;     (t/testing "successes"
;;       (t/is (= {:badger nil :bat nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:badger nil :bat "X"}
;;                (sut/parse-args parser ["--bat" "X"])))

;;       ;; we don't support partial match
;;       (t/is (thrown? Exception (sut/parse-args parser ["--bad"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--badg"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--badge"])))

;;       (t/is (= {:badger true :bat nil}
;;                (sut/parse-args parser ["--badger"]))))))

;; (t/deftest test-optionals-double-dash-prefix-match
;;   (let [parser (-> {}
;;                    (sut/add-argument "--badger" :action (constantly true))
;;                    (sut/add-argument "--ba"))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["--bar"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--b"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--ba"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--b=2"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--badge" "5"]))))

;;     (t/testing "successes"
;;       (t/is (= {:badger nil :ba nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:badger nil :ba "X"}
;;                (sut/parse-args parser ["--ba" "X"])))

;;       (t/is (= {:badger nil :ba "X"}
;;                (sut/parse-args parser ["--ba=X"])))

;;       ;; we don't support partial match
;;       (t/is (thrown? Exception (sut/parse-args parser ["--bad"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--badg"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--badge"])))

;;       (t/is (= {:badger true :ba nil}
;;                (sut/parse-args parser ["--badger"]))))))

;; (t/deftest test-optionals-single-double-dash
;;   (let [parser (-> {}
;;                    (sut/add-argument "-f" :action (constantly true))
;;                    (sut/add-argument "--bar")
;;                    (sut/add-argument "-baz" :action (constantly 42)))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["--bar"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-fbar"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-fbaz"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-bazf"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-b" "B"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["B"]))))

;;     (t/testing "successes"
;;       (t/is (= {:f nil :bar nil :baz nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:f true :bar nil :baz nil}
;;                (sut/parse-args parser ["-f"])))

;;       (t/is (thrown? Exception (sut/parse-args parser ["--ba" "B"])))

;;       (t/is (= {:f true :bar "B" :baz nil}
;;                (sut/parse-args parser ["-f" "--bar" "B"])))

;;       (t/is (thrown? Exception (sut/parse-args parser ["-f" "-b"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-ba" "-f"]))))))

;; ;; we don't support :prefix-chars
;; #_(t/deftest test-optionals-alternate-prefix-chars
;;   (let [parser (-> {:prefix-chars "+:/"}
;;                    (sut/add-argument "+f" :action (constantly true))
;;                    (sut/add-argument "::bar")
;;                    (sut/add-argument "/baz" :action (constantly 42)))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["--bar"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-fbar"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-b" "B"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["B"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-f"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--bar" "B"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-baz"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-h"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["--help"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["+h"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["::help"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["/help"]))))

;;     (t/testing "successes"
;;       (t/is (= {:f nil :bar nil :baz nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:f true :bar nil :baz nil}
;;                (sut/parse-args parser ["+f"])))

;;       (t/is (= {:f nil :bar "B" :baz nil}
;;                (sut/parse-args parser ["::ba" "B"])))

;;       (t/is (= {:f true :bar "B" :baz nil}
;;                (sut/parse-args parser ["+f" "::bar" "B"])))

;;       (t/is (= {:f true :bar nil :baz 42}
;;                (sut/parse-args parser ["+f" "/b"])))

;;       (t/is (= {:f true :bar nil :baz 42}
;;                (sut/parse-args parser ["/ba" "+f"]))))))

;; (t/deftest test-optionals-short-long
;;   (let [parser (-> {}
;;                    (sut/add-argument ["-v" "--verbose" "-n" "--noisy"]
;;                                      :action (constantly true)))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["--x" "--verbose"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-N"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["a"])))
;;       (t/is (thrown? Exception (sut/parse-args parser ["-v" "x"]))))

;;     (t/testing "successes"
;;       (t/is (= {:verbose nil}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:verbose true}
;;                (sut/parse-args parser ["-v"])))

;;       (t/is (= {:verbose true}
;;                (sut/parse-args parser ["--verbose"])))

;;       (t/is (= {:verbose true}
;;                (sut/parse-args parser ["-n"])))

;;       (t/is (= {:verbose true}
;;                (sut/parse-args parser ["--noisy"]))))))

;; (t/deftest test-optionals-dest
;;   (let [parser (-> {}
;;                    (sut/add-argument ["--foo-bar"])
;;                    (sut/add-argument ["--baz"] :dest :zabbaz))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["a"]))))

;;     (t/testing "successes"
;;       (t/is (= {:foo-bar "f" :zabbaz nil}
;;                (sut/parse-args parser ["--foo-bar" "f"])))

;;       (t/is (= {:foo-bar nil :zabbaz "g"}
;;                (sut/parse-args parser ["--baz" "g"])))

;;       (t/is (= {:foo-bar "h" :zabbaz "i"}
;;                (sut/parse-args parser ["--foo-bar" "h" "--baz" "i"])))

;;       (t/is (= {:foo-bar "k" :zabbaz "j"}
;;                (sut/parse-args parser ["--baz" "j" "--foo-bar" "k"]))))))

;; (t/deftest test-optionals-default
;;   (let [parser (-> {}
;;                    (sut/add-argument ["-x"])
;;                    (sut/add-argument ["-y"] :default 42))]

;;     (t/testing "failures"
;;       (t/is (thrown? Exception (sut/parse-args parser ["a"]))))

;;     (t/testing "successes"
;;       (t/is (= {:x nil :y 42}
;;                (sut/parse-args parser [])))

;;       (t/is (= {:x "x" :y 42}
;;                (sut/parse-args parser ["-x" "x"])))

;;       (t/is (= {:x nil :y "y"}
;;                (sut/parse-args parser ["-y" "y"]))))))

;; (t/deftest test-optionals-nargs-default
;;   (t/testing "Tests not specifying the number of args for an Optional"
;;     (let [parser (-> {}
;;                      (sut/add-argument ["-x"]))]

;;       (t/testing "failures"
;;         (t/is (thrown? Exception (sut/parse-args parser ["a"])))
;;         (t/is (thrown? Exception (sut/parse-args parser ["-x"]))))

;;       (t/testing "successes"
;;         (t/is (= {:x nil}
;;                  (sut/parse-args parser [])))

;;         (t/is (= {:x "a"}
;;                  (sut/parse-args parser ["-x" "a"])))))))

;; (t/deftest test-optionals-nargs-1
;;   (t/testing "Tests specifying 1 arg for an Optional"
;;     (let [parser (-> {}
;;                      (sut/add-argument ["-x"] :nargs 1))]

;;       (t/testing "failures"
;;         (t/is (thrown? Exception (sut/parse-args parser ["a"])))
;;         (t/is (thrown? Exception (sut/parse-args parser ["-x"]))))

;;       (t/testing "successes"
;;         (t/is (= {:x nil}
;;                  (sut/parse-args parser [])))

;;         (t/is (= {:x ["a"]}
;;                  (sut/parse-args parser ["-x" "a"])))))))

;; (t/deftest test-optionals-nargs-3
;;   (t/testing "Tests specifying 3 args for an Optional"
;;     (let [parser (-> {}
;;                      (sut/add-argument ["-x"] :nargs 3))]

;;       (t/testing "failures"
;;         (t/is (thrown? Exception (sut/parse-args parser ["a"])))
;;         (t/is (thrown? Exception (sut/parse-args parser ["-x"])))
;;         (t/is (thrown? Exception (sut/parse-args parser ["-x" "a"])))
;;         (t/is (thrown? Exception (sut/parse-args parser ["-x" "a" "b"])))
;;         (t/is (thrown? Exception (sut/parse-args parser ["a" "-x"])))
;;         (t/is (thrown? Exception (sut/parse-args parser ["a" "-x" "b"]))))

;;       (t/testing "successes"
;;         (t/is (= {:x nil}
;;                  (sut/parse-args parser [])))

;;         (t/is (= {:x ["a" "b" "c"]}
;;                  (sut/parse-args parser ["-x" "a" "b" "c"])))))))
