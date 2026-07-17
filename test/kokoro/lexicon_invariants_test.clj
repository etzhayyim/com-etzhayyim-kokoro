(ns kokoro.lexicon-invariants-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]))

(defn lexicon [stem]
  (edn/read-string (slurp (io/file "lex" (str stem ".edn")))))
(defn record-node [stem] (get-in (lexicon stem) ["defs" "main" "record"]))
(def zero-counters
  ["clinicalPsychiatricEntityPenetrationPct" "videoRecordingEventsCount"
   "conversionTherapyEventsCount" "aiOnlyTherapyEventsCount"
   "commercialMentalHealthSoftwarePenetrationPct"
   "commercialAiTherapyChatbotPenetrationPct" "mandatoryScreeningEventsCount"
   "surveillanceBasedMoodMonitoringEventsCount"])

(defn nodes [x]
  (tree-seq #(or (map? %) (sequential? %))
            #(cond (map? %) (vals %) (sequential? %) % :else nil)
            x))

(deftest session-content-requires-encrypted-cids
  (doseq [[stem field] [["griefSupportAttestation" "encryptedPayloadCid"]
                        ["peerSupportCircleAttestation" "encryptedPayloadCid"]
                        ["acuteCrisisEscalationLog" "encryptedContextCid"]]]
    (is (contains? (set (get (record-node stem) "required")) field) stem))
  (is (contains? (set (get (record-node "griefSupportAttestation") "required"))
                 "bereavedPseudonymDid"))
  (is (contains? (set (get (record-node "acuteCrisisEscalationLog") "required"))
                 "affectedPseudonymDid")))

(deftest ethics-red-lines-remain-structural
  (let [props (get (record-node "silenKokoroReview") "properties")]
    (doseq [field zero-counters]
      (is (zero? (get-in props [field "const"])) field))
    (is (= 10000
           (get-in props ["counselorVocationFlowCompliantRatioPctIntegerHundredths" "const"])))))

(deftest free-conscience-and-no-surveillance
  (let [props (get (record-node "peerSupportCircleAttestation") "properties")]
    (is (true? (get-in props ["optInOnly" "const"])))
    (is (false? (get-in props ["surveillanceBasedMonitoring" "const"])))))

(deftest lexicon-hygiene
  (doseq [file (filter #(and (.isFile %) (.endsWith (.getName %) ".edn"))
                       (file-seq (io/file "lex")))]
    (let [stem (.replace (.getName file) ".edn" "")
          doc (edn/read-string (slurp file))]
      (testing stem
        (is (= (str "com.etzhayyim.kokoro." stem) (get doc "id")))
        (is (not-any? #(and (map? %) (= "number" (get % "type"))) (nodes doc)))))))
