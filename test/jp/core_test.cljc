(ns jp.core-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [are deftest is testing run-tests]])
            #?(:cljs [cljs.reader])
            #?(:cljs [doo.runner :refer-macros [doo-tests]])
            [jp.core :as jp]))


(deftest test-all-paths

  (testing "given an empty map returns an empty vector"
    (is (= []
           (jp/all-paths {}))))

  (testing "given a simple (single level) map returns its keys"
    (is (= (mapv vector [:foo :baz :bam])
           (jp/all-paths {:foo :bar
                          :baz :bif
                          :bam :boom}))))

  (testing "given a nested map returns all of its korks"
    (is (= [[:foo]
            [:baz :bif]
            [:baz :boom :bop]
            [:baz :boom :y]
            [:w]]
           (jp/all-paths {:foo :bar
                          :baz {:bif :bam
                                :boom {:bop :blorp
                                       :y :z}}
                          :w :x})))))


(deftest test-args->opts
  (testing "given nil"
    (let [args nil]

      (testing "returns an empty map"
        (is (= {}
               (jp/args->opts args))))))

  (testing "given args"
    (let [args [:foo "bar" :baz {:bif 5}]]

      (testing "returns an appropriate map of those args"
        (is (= {:foo "bar"
                :baz {:bif 5}}))))))


(deftest test-call-back
  (testing "given a function that does not throw an exception"
    (let [f *]

      (testing "returns the value of the function on the args"
        (let [result (jp/call-back f 3 5)]
          (is (= 15
                 result))))))

  (testing "given a function that throws an exception"
    (let [explode (fn [& args] (throw (ex-info "boom" {:args args})))]

      (testing "returns nil"
        (let [result (jp/call-back explode 3 5)]
          (is (= nil
                 result)))))))


(deftest test-contains-all?
  (testing "given a map that contains all keys"
    (let [m {:a 1 :b 2 :c 3}]

      (testing "returns true"
        (is (jp/contains-all? m [:a :b :c]))
        (is (jp/contains-all? m [:a :b]))
        (is (jp/contains-all? m [:a :c]))
        (is (jp/contains-all? m [:b :c]))
        (is (jp/contains-all? m [:a]))
        (is (jp/contains-all? m [:b]))
        (is (jp/contains-all? m [:c])))))

  (testing "given a map that does not contain all keys"
    (let [m {:a 1 :c 3}]

      (testing "returns false"
        (is (not (jp/contains-all? m [:a :b :c])))
        (is (not (jp/contains-all? m [:b :c])))
        (is (not (jp/contains-all? m [:a :b])))
        (is (not (jp/contains-all? m [:b])))))))


(deftest test-deep-merge-with
  (testing "does what it says on the tin"
    (is (= {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}
           (jp/deep-merge-with +
                               {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                               {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})))))



(deftest test-filterm
  (testing "basics"
    (is (= {:a :b :e :f}
           (jp/filterm (fn [[k v]] (not= k v))
                       {:a :b :c :c :e :f})))))


(deftest test-keys-in
  (testing "gives you the keys you crave"
    (is (= '([:a :b]
             [:a :c]
             [:d :e :f])
           (jp/keys-in {:a {:b 5
                            :c 6}
                        :d {:e {:f 7}}})))))


(deftest test-not-implemented!
  (testing "throws an exception"
    (is (thrown? #?(:clj Exception :cljs :default)
                 (jp/not-implemented! :testing)))))


(deftest test-seq!
  (testing "when given nil"
    (testing "returns nil"
      (is (nil? (jp/seq! nil)))))

  (testing "when given a sequence"
    (let [sequences [[]
                     [1 2 3 4]
                     '()
                     (list :a :b :c)]]

      (testing "returns it untouched"
        (doseq [s sequences]
          (is (= s
                 (jp/seq! s)))))))

  (testing "when given a non-sequence"
    (let [non-sequences [{:a :b}
                         5
                         "foo"]]

      (testing "returns it wrapped in a vector"
        (doseq [x non-sequences]
          (is (= [x]
                 (jp/seq! x))))))))


(deftest test-enumerate
  (testing "enumerates as in python"
    (is (= '([0 :a] [1 :b] [2 :c])
           (jp/enumerate [:a :b :c])))))


(deftest test-ensure-with
  (letfn [(read-s [x]
            (#?(:clj read-string
                :cljs cljs.reader/read-string) x))]
    (testing "when pred fails"
      (testing "returns (f x)"
        (is (= 5
               (jp/ensure-with (comp not string?) read-s "5")))))

    (testing "when pred succeeds"
      (testing "returns x"
        (is (= 6
               (jp/ensure-with (comp not string?) read-s 6)))))))


(deftest test-sortv-by
  (testing "basics"
    (let [items-list '({:a 3} {:a 1} {:a 4} {:a 1} {:a 5} {:a 9})
          items-vec  (vec items-list)
          sorted-list (sort-by     :a items-vec)
          sorted-vec  (jp/sortv-by :a items-list)]
      (is (= sorted-list
             sorted-vec))
      (is (vector? sorted-vec)))))


(deftest test-sort-table-by
  (testing "basics"
    (is (= [{:id 2 :name "a"}
            {:id 1 :name "x"}]
           (jp/sort-table-by :name
                             {1 {:id 1 :name "x"}
                              2 {:id 2 :name "a"}})))))


(defn std-truncate-test [s sz suffix expected]
  (is (= expected
         (jp/truncate s sz suffix))))


(deftest test-truncate
  (let [s "ABCDEFGH"]

    (testing "with a `suff` longer than `sz`"
      (let [suff "..."]

        (testing "explodes"
          (is (thrown? #?(:clj Exception :cljs js/Error)
                       (jp/truncate s 2 suff))))))

    (testing "with supplied suffix"
      (let [suff "abc"]

        (are [sz ex] (= ex (jp/truncate s sz suff))
          3   "abc"
          4   "Aabc"
          5   "ABabc"
          6   "ABCabc"
          7   "ABCDabc"
          8   "ABCDEFGH"
          9   "ABCDEFGH"
          10  "ABCDEFGH")))

    (testing "with default suffix"

      (are [sz ex] (= ex (jp/truncate s sz))
        3  "..."
        4  "A..."
        5  "AB..."
        6  "ABC..."
        7  "ABCD..."
        8  "ABCDEFGH"
        9  "ABCDEFGH"
        10 "ABCDEFGH"))))


;; (run-tests)
#?(:cljs (doo-tests 'jp.core-test))
