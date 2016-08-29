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
