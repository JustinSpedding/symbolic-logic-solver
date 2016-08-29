(ns symbolic-logic-solver.proof-generator
  (:require [symbolic-logic-solver.statements :refer :all]))

(defn entails? [assumptions conclusion]
  (not (apply consistent? (conj assumptions (->Not conclusion)))))
