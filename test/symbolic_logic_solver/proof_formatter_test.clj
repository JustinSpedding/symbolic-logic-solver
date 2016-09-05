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
