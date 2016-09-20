(ns symbolic-logic-solver.proof-formatter-test
  (:require [clojure.test :refer :all]
            [symbolic-logic-solver.proof-formatter :as formatter]
            [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all]))

(deftest statement->string-test
  (testing "correctly converts statements into strings"
    (is (= "p" (formatter/statement->string (->Var \p))))
    (is (= "(p&q)" (formatter/statement->string (->And (->Var \p) (->Var \q)))))
    (is (= "(pvq)" (formatter/statement->string (->Or  (->Var \p) (->Var \q)))))
    (is (= "(p=q)" (formatter/statement->string (->Equ (->Var \p) (->Var \q)))))
    (is (= "(p>q)" (formatter/statement->string (->Ent (->Var \p) (->Var \q)))))
    (is (= "~p" (formatter/statement->string (->Not (->Var \p)))))
    (is (= "~(p&q)" (formatter/statement->string (->Not (->And (->Var \p) (->Var \q))))))
    (is (= "~(pvq)" (formatter/statement->string (->Not (->Or  (->Var \p) (->Var \q))))))
    (is (= "~(p=q)" (formatter/statement->string (->Not (->Equ (->Var \p) (->Var \q))))))
    (is (= "~(p>q)" (formatter/statement->string (->Not (->Ent (->Var \p) (->Var \q))))))
    (is (= "((p&q)v(r=s))" (formatter/statement->string (->Or (->And (->Var \p) (->Var \q)) (->Equ (->Var \r) (->Var \s))))))))

(deftest AndElimination->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list "test"))]
      (is (= (list (formatter/->Line "p"
                                     "&E %d"
                                     (list (formatter/->StatementReference "(p&q)"))
                                     0)
                   "test")
             (formatter/AndElimination->lines (->AndElimination (->Reiteration (->And (->Var \p)
                                                                                      (->Var \q)))
                                                                (->Var \p))))))))

(deftest OrElimination->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list (formatter/->Line "p" "R %d" (list "p") 0)
                                                      (formatter/->AssumptionLine "p" 0)))]
      (is (= (list (formatter/->Line "r"
                                     "vE %d, %d-%d, %d-%d"
                                     (list (formatter/->StatementReference "(pvq)")
                                           (formatter/->AssumptionReference "p")
                                           (formatter/->InnerAssumptionReference "r" "p")
                                           (formatter/->AssumptionReference "q")
                                           (formatter/->InnerAssumptionReference "r" "q"))
                                     0)
                   (formatter/->Line "p" "R %d" (list "p") 1)
                   (formatter/->AssumptionLine "p" 1)
                   (formatter/->Line "p" "R %d" (list "p") 1)
                   (formatter/->AssumptionLine "p" 1)
                   (formatter/->Line "p" "R %d" (list "p") 0)
                   (formatter/->AssumptionLine "p" 0))
             (formatter/OrElimination->lines (->OrElimination (->Reiteration (->Or (->Var \p)
                                                                                   (->Var \q)))
                                                              (->Assumption (->Var \p)
                                                                            (->Reiteration (->Var \r)))
                                                              (->Assumption (->Var \q)
                                                                            (->Reiteration (->Var \r)))
                                                              (->Var \r))))))))
(deftest EquElimination->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list (formatter/->Line "p" "R %d" (list "p") 0)))]
      (is (= (list (formatter/->Line "q"
                                     "=E %d, %d"
                                     (list (formatter/->StatementReference "(p=q)")
                                           (formatter/->StatementReference "p"))
                                     0)
                   (formatter/->Line "p" "R %d" (list "p") 0)
                   (formatter/->Line "p" "R %d" (list "p") 0))
             (formatter/EquElimination->lines (->EquElimination (->Reiteration (->Equ (->Var \p)
                                                                                      (->Var \q)))
                                                                (->Reiteration (->Var \p))
                                                                (->Var \q))))))))

(deftest EntElimination->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list (formatter/->Line "p" "R %d" (list "p") 0)))]
      (is (= (list (formatter/->Line "q"
                                     ">E %d, %d"
                                     (list (formatter/->StatementReference "(p>q)")
                                           (formatter/->StatementReference "p"))
                                     0)
                   (formatter/->Line "p" "R %d" (list "p") 0)
                   (formatter/->Line "p" "R %d" (list "p") 0))
             (formatter/EntElimination->lines (->EntElimination (->Reiteration (->Ent (->Var \p)
                                                                                      (->Var \q)))
                                                                (->Reiteration (->Var \p))
                                                                (->Var \q))))))))

(deftest NotElimination->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list (formatter/->Line "p" "R %d" (list "p") 0)))]
      (is (= (list (formatter/->Line "p"
                                     "~E %d"
                                     (list (formatter/->StatementReference "~~p"))
                                     0)
                   (formatter/->Line "p" "R %d" (list "p") 0))
             (formatter/NotElimination->lines (->NotElimination (->Reiteration (->Not (->Not (->Var \p))))
                                                                (->Var \p))))))))

(deftest AndIntroduction->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list (formatter/->Line "p" "R %d" (list "p") 0)))]
      (is (= (list (formatter/->Line "(p&q)"
                                     "&I %d, %d"
                                     (list (formatter/->StatementReference "p")
                                           (formatter/->StatementReference "q"))
                                     0)
                   (formatter/->Line "p" "R %d" (list "p") 0)
                   (formatter/->Line "p" "R %d" (list "p") 0))
             (formatter/AndIntroduction->lines (->AndIntroduction (->Reiteration (->Var \p))
                                                                  (->Reiteration (->Var \q))
                                                                  (->And (->Var \p)
                                                                         (->Var \q)))))))))

(deftest OrIntroduction->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list (formatter/->Line "p" "R %d" (list "p") 0)))]
      (is (= (list (formatter/->Line "(pvq)"
                                     "vI %d"
                                     (list (formatter/->StatementReference "p"))
                                     0)
                   (formatter/->Line "p" "R %d" (list "p") 0))
             (formatter/OrIntroduction->lines (->OrIntroduction (->Reiteration (->Var \p))
                                                                (->Or (->Var \p)
                                                                      (->Var \q)))))))))

(deftest EquIntroduction->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list (formatter/->Line "p" "R %d" (list "p") 0)
                                                      (formatter/->AssumptionLine "p" 0)))]
      (is (= (list (formatter/->Line "(p=q)"
                                     "=I %d-%d, %d-%d"
                                     (list (formatter/->AssumptionReference "p")
                                           (formatter/->InnerAssumptionReference "q" "p")
                                           (formatter/->AssumptionReference "q")
                                           (formatter/->InnerAssumptionReference "p" "q"))
                                     0)
                   (formatter/->Line "p" "R %d" (list "p") 1)
                   (formatter/->AssumptionLine "p" 1)
                   (formatter/->Line "p" "R %d" (list "p") 1)
                   (formatter/->AssumptionLine "p" 1))
             (formatter/EquIntroduction->lines (->EquIntroduction (->Assumption (->Var \p)
                                                                                (->Reiteration (->Var \q)))
                                                                  (->Assumption (->Var \q)
                                                                                (->Reiteration (->Var \p)))
                                                                  (->Equ (->Var \p)
                                                                         (->Var \q)))))))))

(deftest EntIntroduction->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list (formatter/->Line "p" "R %d" (list "p") 0)
                                                      (formatter/->AssumptionLine "p" 0)))]
      (is (= (list (formatter/->Line "(p>q)"
                                     ">I %d-%d"
                                     (list (formatter/->AssumptionReference "p")
                                           (formatter/->InnerAssumptionReference "q" "p"))
                                     0)
                   (formatter/->Line "p" "R %d" (list "p") 1)
                   (formatter/->AssumptionLine "p" 1))
             (formatter/EntIntroduction->lines (->EntIntroduction (->Assumption (->Var \p)
                                                                                (->Reiteration (->Var \q)))
                                                                  (->Ent (->Var \p)
                                                                         (->Var \q)))))))))

(deftest NotIntroduction->lines-test
  (testing "converts to line correctly"
    (with-redefs [formatter/step->lines (fn [_] (list (formatter/->Line "~q" "R %d" (list "~q") 0)
                                                      (formatter/->Line "q" "R %d" (list "q") 0)
                                                      (formatter/->AssumptionLine "p" 0)))]
      (is (= (list (formatter/->Line "~p"
                                     "~I %d(%d, %d)"
                                     (list (formatter/->AssumptionReference "p")
                                           (formatter/->InnerAssumptionReference "q" "p")
                                           (formatter/->InnerAssumptionReference "~q" "p"))
                                     0)
                   (formatter/->Line "~q" "R %d" (list "~q") 1)
                   (formatter/->Line "q" "R %d" (list "q") 1)
                   (formatter/->AssumptionLine "p" 1))
             (formatter/NotIntroduction->lines (->NotIntroduction (->Contradiction (->Var \p)
                                                                                   (->Reiteration (->Var \q))
                                                                                   (->Reiteration (->Not (->Var \q))))
                                                                  (->Not (->Var \p)))))))))

(deftest Reiteration->lines-test
  (testing "converts to line correctly"
    (is (= (list (formatter/->Line "p"
                                   "R %d"
                                   (list (formatter/->ReiterationReference "p"))
                                   0))
           (formatter/Reiteration->lines (->Reiteration (->Var \p)))))))

(deftest Assumption->lines-test
  (testing "converts to line correctly"
    (is (= (list (formatter/->Line "p"
                                   "R %d"
                                   (list (formatter/->ReiterationReference "p"))
                                   0)
                 (formatter/->AssumptionLine "p" 0))
           (formatter/Assumption->lines (->Assumption (->Var \p)
                                                      (->Reiteration (->Var \p))))))))

(deftest Contradiction->lines-test
  (testing "converts to line correctly"
    (is (= (list (formatter/->Line "~q"
                                   "R %d"
                                   (list (formatter/->ReiterationReference "~q"))
                                   0)
                 (formatter/->Line "q"
                                   "R %d"
                                   (list (formatter/->ReiterationReference "q"))
                                   0)
                 (formatter/->AssumptionLine "p" 0))
           (formatter/Contradiction->lines (->Contradiction (->Var \p)
                                                            (->Reiteration (->Var \q))
                                                            (->Reiteration (->Not (->Var \q)))))))))
