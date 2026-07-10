(ns kokoro.murakumo
  "Pure cljc actor boundary generated from manifest migration scaffold."
  (:require [clojure.string :as str]))

(def actor-did
  "did:web:kokoro.etzhayyim.com")

(def common-gates
  [:council-charter-attestation
   :no-platform-held-key-baseline
   :no-probing-baseline
   :murakumo-only-inference-baseline
   :did-primary-baseline
   :append-only-gate-baseline
   :kotoba-only-substrate-baseline])

(defn collection
  [name]
  (str "com.etzhayyim.kokoro." name))

(def cell-specs {
  :peer_support_circle {:legacy-cell "peer-support-circle"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "peer_support_circle")]
     :required-gates common-gates
     :trigger "manifest cell peer_support_circle"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :grief_support {:legacy-cell "grief-support"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "grief_support")]
     :required-gates common-gates
     :trigger "manifest cell grief_support"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :chronic_mental_health_continuity {:legacy-cell "chronic-mental-health-continuity"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "chronic_mental_health_continuity")]
     :required-gates common-gates
     :trigger "manifest cell chronic_mental_health_continuity"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :postnatal_mood_screening {:legacy-cell "postnatal-mood-screening"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "postnatal_mood_screening")]
     :required-gates common-gates
     :trigger "manifest cell postnatal_mood_screening"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :acute_crisis_escalation {:legacy-cell "acute-crisis-escalation"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "acute_crisis_escalation")]
     :required-gates common-gates
     :trigger "manifest cell acute_crisis_escalation"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :counseling_referral {:legacy-cell "counseling-referral"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "counseling_referral")]
     :required-gates common-gates
     :trigger "manifest cell counseling_referral"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
})

(defn safe-rkey
  [s]
  (let [clean (-> (str s)
                  (str/replace #"^did:web:" "")
                  (str/replace #"[^A-Za-z0-9._~-]" "-"))]
    (if (str/blank? clean) "unknown" clean)))

(defn gate-value
  [attestations gate]
  (or (get attestations gate)
      (get attestations (name gate))
      (when (set? attestations) (attestations gate))
      (when (set? attestations) (attestations (name gate)))))

(defn missing-gates
  [spec attestations]
  (->> (:required-gates spec)
       (remove #(boolean (gate-value attestations %)))
       vec))

(defn put-record-effect
  [collection rkey record]
  {:op :mst/put-record
   :actor actor-did
   :collection collection
   :rkey rkey
   :record record})

(defn records-for
  [spec {:keys [records record computed-at request-id]
         :as input}]
  (let [input-records (cond
                        (map? records) records
                        (some? record) {0 record}
                        :else {})
        base {:actorDid actor-did
              :computedAt computed-at
              :legacyCell (:legacy-cell spec)
              :phase (:phase spec)
              :requestId request-id
              :actorBoundary "cljc-migration-scaffold"
              :scaffold true
              :constitutionalStatus "attested-plan"}]
    (map-indexed
     (fn [idx coll]
       (let [record* (merge {:$type coll}
                            base
                            (or (get input-records coll)
                                (get input-records idx)
                                {}))
             rkey (safe-rkey (or (:rkey record*)
                                 (get record* "rkey")
                                 (:tid record*)
                                 request-id
                                 (str (:legacy-cell spec) "-" idx)))]
         {:collection coll
          :record record*
          :rkey rkey}))
     (:collections spec))))

(defn cell-plan
  [cell-key {:keys [attestations] :as input}]
  (let [spec (get cell-specs cell-key)]
    (when-not spec
      (throw (ex-info "unknown cell" {:cell cell-key})))
    (let [missing (missing-gates spec attestations)]
      (merge
       {:cell cell-key
        :legacy-cell (:legacy-cell spec)
        :actor actor-did
        :phase (:phase spec)
        :murakumo-node (:murakumo-node spec)
        :trigger (:trigger spec)
        :ceiling (:ceiling spec)
        :required-gates (:required-gates spec)
        :missing-gates missing}
       (if (seq missing)
         {:status :blocked
          :effects []}
         (let [planned-records (records-for spec input)]
           {:status :ready
            :records (vec planned-records)
            :effects (mapv (fn [{:keys [collection record rkey]}]
                             (put-record-effect collection rkey record))
                           planned-records)}))))))

(defn all-cell-plans
  [input]
  (into {}
        (map (fn [cell-key] [cell-key (cell-plan cell-key input)]))
        (keys cell-specs)))
