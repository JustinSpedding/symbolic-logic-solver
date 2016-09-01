(ns symbolic-logic-solver.statements-test
  (:require [clojure.test :refer :all]
            [symbolic-logic-solver.statements :refer :all]))

(deftest find-contradiction-test
  (testing "returns a contradicting variable if one exists"
    (is (= (->Var \p)
           (find-contradiction (list (->Var \p)
                                     (->Not (->Var \p)))))))

  (testing "returns nil if there is no contradiction"
    (is (not (find-contradiction (list (->Var \p)
                                       (->Var \q)))))))

(deftest consistent?-test
  (testing "corrently identifies consistent groups of statements"
    (is (consistent?))

    (is (consistent? (->Var \p)))

    (is (consistent? (->Var \p)
                     (->Var \q)))

    (is (consistent? (->Var \p)
                     (->Var \q)
                     (->Not (->Var \r))))

    (is (consistent? (->Var \p)
                     (->Var \q)
                     (->Not (->Var \r))
                     (->Not (->Var \s))))

    (is (consistent? (->Var \p)
                     (->Var \p)))

    (is (consistent? (->Not (->Var \p))
                     (->Not (->Var \q))))

    (is (consistent? (->And (->Var \p)
                            (->Var \q))
                     (->Var \p)
                     (->Var \q)))

    (is (consistent? (->Or (->Var \p)
                           (->Var \q))
                     (->Var \p)
                     (->Var \q)))

    (is (consistent? (->Or (->Var \p)
                           (->Var \q))
                     (->Not (->Var \q))))

    (is (consistent? (->Or (->Var \p)
                           (->Var \q))
                     (->Not (->Var \p))))

    (is (consistent? (->Equ (->Var \p)
                            (->Var \q))
                     (->Var \p)
                     (->Var \q)))

    (is (consistent? (->Ent (->Var \p)
                            (->Var \q))
                     (->Var \p)
                     (->Var \q))))

  (testing "corrently identifies inconsistent groups of statements"
    (is (not (consistent? (->Var \p)
                          (->Not (->Var \p)))))

    (is (not (consistent? (->And (->Var \p)
                                 (->Var \q))
                          (->Not (->Var \p)))))

    (is (not (consistent? (->And (->Var \p)
                                 (->Var \q))
                          (->Not (->Var \q)))))

    (is (not (consistent? (->Or (->Var \p)
                                (->Var \q))
                          (->Not (->Var \p))
                          (->Not (->Var \q)))))

    (is (not (consistent? (->Equ (->Var \p)
                                 (->Var \q))
                          (->Var \p)
                          (->Not (->Var \q)))))

    (is (not (consistent? (->Ent (->Var \p)
                                 (->Var \q))
                          (->Var \p)
                          (->Not (->Var \q)))))

    (is (not (consistent? (->Not (->And (->Var \p)
                                        (->Var \q)))
                          (->Var \p)
                          (->Var \q))))

    (is (not (consistent? (->Not (->Or (->Var \p)
                                       (->Var \q)))
                          (->Var \p))))

    (is (not (consistent? (->Not (->Or (->Var \p)
                                       (->Var \q)))
                          (->Var \q))))

    (is (not (consistent? (->Not (->Equ (->Var \p)
                                        (->Var \q)))
                          (->Var \p)
                          (->Var \q))))

    (is (not (consistent? (->Not (->Equ (->Var \p)
                                        (->Var \q)))
                          (->Not (->Var \p))
                          (->Not (->Var \q)))))

    (is (not (consistent? (->Not (->Ent (->Var \p)
                                        (->Var \q)))
                          (->Not (->Var \p)))))

    (is (not (consistent? (->Not (->Ent (->Var \p)
                                        (->Var \q)))
                          (->Var \q))))))
