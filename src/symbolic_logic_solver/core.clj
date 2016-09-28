(ns symbolic-logic-solver.core
  (:gen-class)
  (:require [symbolic-logic-solver.statement-parser :as parser]
            [symbolic-logic-solver.proof-generator :as generator]
            [symbolic-logic-solver.proof-formatter :as formatter]))

(defn prove [assumptions conclusion]
  (let [assumptions (map parser/string->statement assumptions)
        conclusion (parser/string->statement conclusion)]
    (if (generator/entails? assumptions conclusion)
      (formatter/format-proof assumptions (generator/generate-proof assumptions conclusion))
      "The assumptions do not entail the conclusion.")))

(defn -main [& args]
  (println "Enter your assumptions and conclusion: ")
  (let [input (clojure.string/split (read-line) #" ")]
    (println (prove (butlast input)
                    (last input)))))
