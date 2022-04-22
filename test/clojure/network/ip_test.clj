(ns clojure.network.ip-test
  (:require
   [clojure.test :refer :all]
   [clojure.network.ip :as ip]))

(defn- hard-count
  [a-seq]
  (->> a-seq
       (map (fn [_] 1))
       (reduce + 0)))

(defn- pow [x n]
  (reduce * (repeat n x)))

(deftest test-lazy-network-sequences
  (testing "Network sequences are lazy"
    (testing "120 bit mask"
      (let [ipv6-net (ip/make-network "1000:0000:0000:0001:0000:0000:0000:0000/120")
            ip-seq (seq ipv6-net)]
        (is (= 256 (count ipv6-net)))
        (is (= 256 (hard-count ip-seq)))))
    (testing "112 bit mask"
      (let [ipv6-net (ip/make-network "1000:0000:0000:0001:0000:0000:0000:0000/112")
            ip-seq (seq ipv6-net)]
        (is (= (pow 256 2) (count ipv6-net)))
        (is (= (pow 256 2) (hard-count ip-seq)))))
    ; These test cases should not stall if the sequence is truly lazy.
    (testing "104 bit mask"
      (let [ipv6-net (ip/make-network "1000:0000:0000:0001:0000:0000:0000:0000/104")
            ip-seq #(seq ipv6-net)]
        (is (= (pow 256 3) (count ipv6-net)))
        (is (some? (first (ip-seq))))))
    (testing "64 bit mask"
      (let [ipv6-net (ip/make-network "1000:0000:0000:0001:0000:0000:0000:0000/64")
            ip-seq #(seq ipv6-net)]
        ; integer overflow
        ; (is (= (pow 256 8) (count ipv6-net)))
        (is (some? (first (ip-seq))))))))