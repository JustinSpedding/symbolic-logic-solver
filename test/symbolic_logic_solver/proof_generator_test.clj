(ns symbolic-logic-solver.proof-generator-test
  (:require [clojure.test :refer :all]
            [symbolic-logic-solver.proof-generator :as generator]
            [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all]))

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

(deftest eliminate-var-test
  (testing "returns the last step if it reaches the conclusion"
    (is (= (->Reiteration (->Var \p))
           (generator/eliminate-var ()
                                    (->Reiteration (->Var \p))
                                    (->Var \p)))))

  (testing "returns nil if the last step does not reach the conclusion"
    (is (not (generator/eliminate-var ()
                                      (->Reiteration (->Var \p))
                                      (->Var \q))))))

(deftest eliminate-and-test
  (testing "eliminates And if an arg is the conclusion"
    (let [and-statement (->And (->Var \p) (->Var \q))]
      (is (= (->AndElimination (->Reiteration and-statement) (->Var \p))
             (generator/eliminate-and (list and-statement)
                                      (->Reiteration and-statement)
                                      (->Var \p))))

      (is (= (->AndElimination (->Reiteration and-statement) (->Var \q))
             (generator/eliminate-and (list and-statement)
                                      (->Reiteration and-statement)
                                      (->Var \q))))

      (is (= (->AndElimination (->Reiteration and-statement) (->Var \p))
             (generator/eliminate-and (list and-statement
                                            (->And and-statement (->Var \r)))
                                      (->Reiteration and-statement)
                                      (->Var \p)))))))

  (testing "does not eliminate And if neither arg is the conclusion"
      (let [and-statement (->And (->Var \p) (->Var \q))]
        (is (not (generator/eliminate-and (list and-statement (->Var \r))
                                          (->Reiteration and-statement)
                                          (->Var \r))))))

  (deftest eliminate-or-test
    (testing "eliminates Or if both args entail the conclusion"
      (let [or-statement (->Or (->Var \p) (->Var \p))]
        (is (= (->OrElimination (->Reiteration or-statement)
                                (->Assumption (->Var \p) (->Reiteration (->Var \p)))
                                (->Assumption (->Var \p) (->Reiteration (->Var \p)))
                                (->Var \p))
               (generator/eliminate-or (list or-statement)
                                       (->Reiteration or-statement)
                                       (->Var \p)))))))

(deftest eliminate-equ-test
  (testing "eliminates Equ if one arg is the conclusion and the other is entailed by the assumptions"
    (let [equ-statement (->Equ (->Var \p) (->Var \q))]
      (is (= (->EquElimination (->Reiteration equ-statement)
                               (->Reiteration (->Var \p))
                               (->Var \q))
             (generator/eliminate-equ (list equ-statement
                                            (->Var \p))
                                      (->Reiteration equ-statement)
                                      (->Var \q)))))))
