(ns kokoro.registry-seed-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def seed (edn/read-string (slurp "registry/support-lines.seed.edn")))
(def lines (get seed "lines"))
(def support-kinds
  #{"emergency-number" "crisis-hotline" "text-or-chat-line" "youth-line"
    "specialized-line" "intl-directory"})
(def timestamp-pattern #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z$")

(deftest registry-shape-and-unique-ids
  (is (map? seed))
  (is (seq lines))
  (let [ids (map #(get % "lineId") lines)]
    (is (every? seq ids))
    (is (= (count ids) (count (set ids))))))

(deftest entries-ship-fail-closed
  (is (every? #(= "unverified-seed" (get % "verificationStatus")) lines)))

(deftest safety-critical-contact-provenance-is-complete
  (doseq [line lines]
    (testing (get line "lineId")
      (is (seq (str/trim (get line "contact" ""))))
      (is (str/starts-with? (get line "provenance" "") "https://"))
      (is (re-matches timestamp-pattern (get line "lastVerified" ""))))))

(deftest registry-has-worldwide-coverage
  (is (every? #(seq (get % "jurisdiction")) lines))
  (is (<= 12 (count (set (map #(get % "jurisdiction") lines))))))

(deftest support-kinds-remain-closed
  (is (every? #(contains? support-kinds (get % "supportKind")) lines)))

(deftest notes-preserve-non-clinical-routing-boundary
  (doseq [line lines]
    (let [notes (get line "notes" "")]
      (is (str/includes? notes "kokoro") (get line "lineId"))
      (is (str/includes? notes "NOT clinical") (get line "lineId"))
      (is (some #(str/includes? notes %) ["SUPPORT routing" "ROUTES" "routes" "routing"])
          (get line "lineId")))))

(deftest freshness-window-is-positive-integer
  (let [days (get seed "freshnessWindowDays")]
    (is (integer? days))
    (is (pos? days))))
