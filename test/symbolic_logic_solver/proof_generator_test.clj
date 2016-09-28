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

(deftest eliminate-and-test
  (testing "eliminates And when an arg is the conclusion"
    (let [and-statement (->And (->Var \p) (->Var \q))]
      (is (= (->AndElimination (->Reiteration and-statement) (->Var \p))
             (generator/eliminate (list and-statement)
                                  (->Reiteration and-statement)
                                  (->Var \p))))

      (is (= (->AndElimination (->Reiteration and-statement) (->Var \q))
             (generator/eliminate (list and-statement)
                                  (->Reiteration and-statement)
                                  (->Var \q))))

      (is (= (->AndElimination (->AndElimination (->Reiteration (->And and-statement (->Var \r))) and-statement) (->Var \p))
             (generator/eliminate (list (->And and-statement (->Var \r)))
                                  (->Reiteration (->And and-statement (->Var \r)))
                                  (->Var \p))))))

  (testing "does not eliminate And when neither arg is the conclusion"
    (let [and-statement (->And (->Var \p) (->Var \q))]
      (is (not (generator/eliminate (list and-statement
                                          (->Var \r))
                                    (->Reiteration and-statement)
                                    (->Var \r)))))))

  (deftest eliminate-or-test
    (testing "eliminates Or when both args entail the conclusion"
      (let [or-statement (->Or (->Var \p) (->Var \p))]
        (is (= (->OrElimination (->Reiteration or-statement)
                                (->Assumption (->Var \p) (->Reiteration (->Var \p)))
                                (->Assumption (->Var \p) (->Reiteration (->Var \p)))
                                (->Var \p))
               (generator/eliminate (list or-statement)
                                    (->Reiteration or-statement)
                                    (->Var \p)))))

      (let [and-statement (->And (->Var \p) (->Var \q))
            or-statement (->Or and-statement
                               and-statement)
            and-step (->AndElimination (->Reiteration and-statement) (->Var \p))]
        (with-redefs [generator/generate-proof (fn [a b] and-step)]
          (is (= (->OrElimination (->Reiteration or-statement)
                                  (->Assumption and-statement and-step)
                                  (->Assumption and-statement and-step)
                                  (->Var \p))
                 (generator/eliminate (list or-statement)
                                      (->Reiteration or-statement)
                                      (->Var \p)))))))

    (testing "does not eliminate Or when at least one arg does not entail the conclusion"
      (let [or-statement (->Or (->Var \p) (->Var \q))]
        (is (not (generator/eliminate (list or-statement)
                                      (->Reiteration or-statement)
                                      (->Var \p))))

        (is (not (generator/eliminate (list or-statement)
                                      (->Reiteration or-statement)
                                      (->Var \q))))

        (is (not (generator/eliminate (list or-statement)
                                      (->Reiteration or-statement)
                                      (->Var \r)))))))

(deftest eliminate-equ-test
  (testing "eliminates Equ when one arg is the conclusion and the other is entailed by the assumptions"
    (let [equ-statement (->Equ (->Var \p) (->Var \q))]
      (is (= (->EquElimination (->Reiteration equ-statement)
                               (->Reiteration (->Var \p))
                               (->Var \q))
             (generator/eliminate (list equ-statement
                                        (->Var \p))
                                  (->Reiteration equ-statement)
                                  (->Var \q))))))

  (testing "does not eliminate Equ when neither arg is the conclusion"
    (let [equ-statement (->Equ (->Var \p) (->Var \q))]
      (is (not (generator/eliminate (list equ-statement
                                          (->Var \p))
                                    (->Reiteration equ-statement)
                                    (->Var \r))))))

  (testing "does not eliminate Equ when neither arg is entailed by the assumptions"
    (let [equ-statement (->Equ (->Var \p) (->Var \q))]
      (is (not (generator/eliminate (list equ-statement)
                                    (->Reiteration equ-statement)
                                    (->Var \p))))

      (is (not (generator/eliminate (list equ-statement)
                                    (->Reiteration equ-statement)
                                    (->Var \q)))))))

(deftest eliminate-ent-test
  (testing "eliminates Ent when arg2 is the conclusion and arg1 is entailed by the assumptions"
    (let [ent-statement (->Ent (->Var \p) (->Var \q))]
      (is (= (->EntElimination (->Reiteration ent-statement)
                               (->Reiteration (->Var \p))
                               (->Var \q))
             (generator/eliminate (list ent-statement
                                        (->Var \p))
                                  (->Reiteration ent-statement)
                                  (->Var \q))))))

  (testing "does not eliminate Ent when arg2 is not the conclusion"
    (let [ent-statement (->Ent (->Var \p) (->Var \q))]
      (is (not (generator/eliminate (list ent-statement
                                          (->Var \p)
                                          (->Var \r))
                                    (->Reiteration ent-statement)
                                    (->Var \r))))))

  (testing "does not eliminate Ent when arg1 is not entailed by the assumptions"
    (let [ent-statement (->Ent (->Var \p) (->Var \q))]
      (is (not (generator/eliminate (list ent-statement)
                                    (->Reiteration ent-statement)
                                    (->Var \q)))))))

(deftest eliminate-not-test
  (testing "eliminates Not when it is a double negation of the conclusion"
    (let [not-statement (->Not (->Not (->Var \p)))]
      (is (= (->NotElimination (->Reiteration not-statement)
                               (->Var \p))
             (generator/eliminate (list not-statement)
                                  (->Reiteration not-statement)
                                  (->Var \p))))))

  (testing "does not eliminate Not when it is not a double negation of the conclusion"
    (let [not-statement (->Not (->Not (->Var \p)))]
      (is (not (generator/eliminate (list not-statement)
                                    (->Reiteration not-statement)
                                    (->Var \q)))))))

(deftest introduce-and-test
  (testing "introduces And when both args are entailed by the assumptions"
    (let [and-statement (->And (->Var \p) (->Var \q))]
      (with-redefs [generator/generate-proof (fn [a b] "test")]
        (is (= (->AndIntroduction "test" "test" and-statement)
               (generator/introduce-and (list (->Var \p)
                                              (->Var \q))
                                        and-statement))))))

  (testing "does not introduce And when either arg is not entailed by the assumptions"
    (let [and-statement (->And (->Var \p) (->Var \q))]
      (is (not (generator/introduce-and (list)
                                        and-statement)))

      (is (not (generator/introduce-and (list (->Var \p))
                                        and-statement)))

      (is (not (generator/introduce-and (list (->Var \q))
                                        and-statement))))))

(deftest introduce-or-test
  (testing "introduces Or when at least one arg is entailed by the assumptions"
    (let [or-statement (->Or (->Var \p) (->Var \q))]
      (with-redefs [generator/generate-proof (fn [a b] "test")]
        (is (= (->OrIntroduction "test" or-statement)
               (generator/introduce-or (list (->Var \p))
                                       or-statement)))

        (is (= (->OrIntroduction "test" or-statement)
               (generator/introduce-or (list (->Var \q))
                                       or-statement)))

        (is (= (->OrIntroduction "test" or-statement)
               (generator/introduce-or (list (->Var \p)
                                             (->Var \q))
                                       or-statement))))))

  (testing "does not introduce Or when neither arg is entailed by the conclusion"
    (let [or-statement (->Or (->Var \p) (->Var \q))]
      (is (not (generator/introduce-or (list or-statement)
                                       or-statement))))))

(deftest introduce-equ-test
  (testing "introduces Equ when each arg can be used with the assumptions to reach the other"
    (let [equ-statement (->Equ (->Var \p) (->Var \q))]
      (with-redefs [generator/generate-proof (fn [a b] "test")]
        (is (= (->EquIntroduction (->Assumption (->Var \p) "test")
                                  (->Assumption (->Var \q) "test")
                                  equ-statement)
               (generator/introduce-equ (list (->Ent (->Var \p)
                                                     (->Var \q))
                                              (->Ent (->Var \q)
                                                     (->Var \p)))
                                        equ-statement))))))

  (testing "does not introduce Equ when at least one arg can not be used with the assumptions to reach the other"
    (let [equ-statement (->Equ (->Var \p) (->Var \q))]
      (is (not (generator/introduce-equ (list)
                                        equ-statement)))

      (is (not (generator/introduce-equ (list (->Ent (->Var \p)
                                                     (->Var \q)))
                                        equ-statement)))

      (is (not (generator/introduce-equ (list (->Ent (->Var \q)
                                                     (->Var \p)))
                                        equ-statement))))))

(deftest introduce-ent-test
  (testing "introduces Ent when arg1 and the assumptions entail arg2"
    (let [ent-statement (->Ent (->Var \p) (->Var \q))]
      (with-redefs [generator/generate-proof (fn [a b] "test")]
        (is (= (->EntIntroduction (->Assumption (->Var \p) "test") ent-statement)
               (generator/introduce-ent (list (->Var \q))
                                        ent-statement)))

        (is (= (->EntIntroduction (->Assumption (->Var \p) "test") ent-statement)
               (generator/introduce-ent (list (->Not (->Var \p)))
                                        ent-statement))))))

  (testing "does not introduce Ent when arg1 and the assumptons do not entail arg2"
    (let [ent-statement (->Ent (->Var \p) (->Var \q))]
      (is (not (generator/introduce-ent (list)
                                        ent-statement))))))

(deftest introduce-not-test
  (testing "introduces Not when assuming arg1 leads to a contradiction"
    (let [not-statement (->Not (->Or (->Var \p) (->Var \q)))]
      (with-redefs [generator/generate-proof (fn [a b] "test")
                    find-contradiction (fn [a] (->Var \p))]
        (is (= (->NotIntroduction (->Contradiction (:arg1 not-statement)
                                                   "test"
                                                   "test")
                                  not-statement)
               (generator/introduce-not (list (->Not (->Var \p))
                                              (->Not (->Var \p)))
                                        not-statement))))))

  (testing "does not introduce Not when arg1 does not lead to a contradiction"
    (let [not-statement (->Not (->Or (->Var \p) (->Var \q)))]
      (is (not (generator/introduce-not (list (->Not (->Var \p)))
                                        not-statement))))))

(deftest indirect-proof-test
  (testing "uses indirect proof when the assumptions entail conclusion"
    (is (= (->NotElimination (->NotIntroduction (->Contradiction (->Not (->Var \p))
                                                                 (->Reiteration (->Var \p))
                                                                 (->Reiteration (->Not (->Var \p))))
                                                (->Not (->Not (->Var \p))))
                             (->Var \p))
           (generator/indirect-proof (list (->Var \p))
                                     (->Var \p)))))

  (testing "does not use indirect proof when the assumptions do not entail the conclusion"
    (is (not (generator/indirect-proof (list (->Var \q))
                                       (->Var \p))))))

(deftest try-introduction-test
  (testing "prioritizes elimination of specific statements"
    (let [and-statement (->And (->Var \p) (->Var \p))
          or-statement  (->Or  (->Var \p) (->Var \p))]
      (is (= (->AndElimination (->Reiteration and-statement) (->Var \p))
             (generator/try-elimination (list and-statement
                                              or-statement)
                                        (->Var \p))))

      (is (= (->AndElimination (->Reiteration and-statement) (->Var \p))
             (generator/try-elimination (list or-statement
                                              and-statement)
                                        (->Var \p)))))))
