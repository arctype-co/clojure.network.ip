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

(deftest test-parse-ip-address
  (testing "simple IP parses"
    (is (= 4 (ip/version (ip/make-ip-address "1.1.1.1"))))
    (is (= 4 (ip/version (ip/make-ip-address "0.0.0.0"))))))

(deftest test-parse-ipv6-address
  (testing "long form IPv6 parses"
    (is (= 6 (ip/version (ip/make-ip-address "1000:0000:0000:0000:0000:0000:0000:0000"))))
    (is (= 6 (ip/version (ip/make-ip-address "0000:0000:0000:0000:0000:0000:0000:0000"))))))

(deftest test-parse-ip-network
  (testing "simple IP network parses"
    (is (= 4 (ip/version (ip/make-network "1.1.1.1/32"))))
    (is (= 4 (ip/version (ip/make-network "0.0.0.0/32"))))))

(deftest test-parse-ipv6-address
  (testing "long form IPv6 parses"
    (is (= 6 (ip/version (ip/make-network "1000:0000:0000:0000:0000:0000:0000:0000/128"))))
    (is (= 6 (ip/version (ip/make-network "0000:0000:0000:0000:0000:0000:0000:0000/128"))))))

(deftest test-ip-string
  (testing "simple IP address"
    (is (= "1.1.1.1"
           (str (ip/make-ip-address "1.1.1.1")))))
  (testing "Leading zero"
    (is (= "0.0.0.0"
           (str (ip/make-ip-address "0.0.0.0"))))))

(deftest test-ipv6-string
  (testing "simple IP address"
    (is (= "1000:0:0:0:0:0:0:0"
           (str (ip/make-ip-address "1000:0000:0000:0000:0000:0000:0000:0000")))))
  (testing "Leading zero"
    (is (= "0:0:0:0:0:0:0:0"
           (str (ip/make-ip-address "0000:0000:0000:0000:0000:0000:0000:0000"))))))

(deftest test-ip-network-sequence
  (testing "simple IP network"
    (is (= (ip/make-ip-address "1.1.1.1")
           (first (ip/make-network "1.1.1.1/32")))))
  (testing "Leading zero"
    (is (= (ip/make-ip-address "0.0.0.0")
           (first (ip/make-network "0.0.0.0/32"))))))

(deftest test-ipv6-network-sequence
  (testing "long form IPv6 parses"
    (is (= "1000:0:0:0:0:0:0:0"
           (str (first (ip/make-network "1000:0000:0000:0000:0000:0000:0000:0000/128")))))
    (is (= "1000:0:0:0:0:0:0:0"
           (str (first (seq (ip/make-network "1000:0000:0000:0000:0000:0000:0000:0000/128")))))))
  (testing "Leading zero"
    (is (= "0:0:0:0:0:0:0:0"
           (str (first (ip/make-network "0000:0000:0000:0000:0000:0000:0000:0000/128")))))
    (is (= "0:0:0:0:0:0:0:0"
           (str (first (seq (ip/make-network "0000:0000:0000:0000:0000:0000:0000:0000/128"))))))))