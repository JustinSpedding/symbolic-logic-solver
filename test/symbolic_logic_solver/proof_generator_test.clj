(ns symbolic-logic-solver.proof-generator-test
  (:require [clojure.test :refer :all]
            [symbolic-logic-solver.proof-generator :as generator]
            [symbolic-logic-solver.statements :refer :all]))

(deftest entails?-test
  (testing "correctly identifies when the assumptions entail the conclusion"
    (is (generator/entails? (list (->Var \p))
                            (->Var \p)))

    (is (generator/entails? (list (->And (->Var \p) (->Var \q)))
                            (->Var \p)))

    (is (generator/entails? (list (->And (->Var \p) (->Var \q)))
                            (->Var \q))))

  (testing "correctly identifies when the assumptions do not entail the conclusion"
    (is (not (generator/entails? (list (->Var \p))
                                 (->Not (->Var \p)))))

    (is (not (generator/entails? (list (->And (->Var \p) (->Var \q)))
                                 (->Not (->Var \p)))))

    (is (not (generator/entails? (list (->And (->Var \p) (->Var \q)))
                                 (->Not (->Var \q)))))

    (is (not (generator/entails? (list (->Var \p))
                                 (->Var \q))))))
