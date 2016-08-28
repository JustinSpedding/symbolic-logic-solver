(ns symbolic-logic-solver.statements-test
  (:require [clojure.test :refer :all]
            [symbolic-logic-solver.statements :refer :all]))

(deftest atom?-test
  (testing "correctly identifies atomic statements"
    (is (atom? (->Var \p)))
    (is (atom? (->Not (->Var \p)))))
  (testing "correctly identifies  atomic statements"
    (is (not (atom? (->And (->Var \p) (->Var \q)))))
    (is (not (atom? (->Or (->Var \p) (->Var \q)))))
    (is (not (atom? (->Equ (->Var \p) (->Var \q)))))
    (is (not (atom? (->Ent (->Var \p) (->Var \q)))))
    (is (not (atom? (->Not (->Not (->Var \p))))))))
