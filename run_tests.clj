(require '[clojure.test :as t])

(doseq [ns-sym '[kokoro.methods.test-charter-gates
                  kokoro.methods.test-manifest-invariants
                  kokoro.lexicon-invariants-test
                  kokoro.registry-seed-test
                  kokoro.murakumo-test
                  kokoro.repository-contract-test]]
  (require ns-sym))

(let [result (apply t/run-tests
                    '[kokoro.methods.test-charter-gates
                      kokoro.methods.test-manifest-invariants
                      kokoro.lexicon-invariants-test
                      kokoro.registry-seed-test
                      kokoro.murakumo-test
                      kokoro.repository-contract-test])]
  (System/exit (if (zero? (+ (:fail result) (:error result))) 0 1)))
