(ns symbolic-logic-solver.statement-parser-test
  (:require [clojure.test :refer :all]
            [symbolic-logic-solver.statement-parser :as parser]
            [symbolic-logic-solver.statements :refer :all]))

(deftest remove-whitespace-test
  (testing "removes all spaces and keeps non-whitespace chars"
    (is (= (seq "helloworld") (#'parser/remove-whitespace "   hello   world   ")))))

(deftest pop-while-removed-item-makes-predicate-true-test
  (testing "pops all items until the predicate is not true"
    (is (= (list 1 2 3) (#'parser/pop-while-removed-item-makes-predicate-true (list 1 2 3) zero?)))
    (is (= (list 1 2 3) (#'parser/pop-while-removed-item-makes-predicate-true (list 1 2 3) #(== 2 %))))
    (is (= (list 2 3) (#'parser/pop-while-removed-item-makes-predicate-true (list 1 2 3) #(== 1 %))))
    (is (= (list 3) (#'parser/pop-while-removed-item-makes-predicate-true (list 1 2 3) #(< % 3)))))

  (testing "does not break with empty stack"
    (is (= (list) (#'parser/pop-while-removed-item-makes-predicate-true (list) identity)))))

(deftest infix-to-postfix-test
  (testing "does it work"
    (is (= "ppv" (#'parser/infix-to-postfix "pvp")))
    (is (= "pq&" (#'parser/infix-to-postfix "p&q")))
    (is (= "pq>" (#'parser/infix-to-postfix "p>q")))
    (is (= "pq=" (#'parser/infix-to-postfix "p=q")))
    (is (= "ppv" (#'parser/infix-to-postfix "(pvp)")))
    (is (= "p~" (#'parser/infix-to-postfix "~p")))
    (is (= "p~p~v" (#'parser/infix-to-postfix "~pv~p")))
    (is (= "p~~" (#'parser/infix-to-postfix "(~~p)")))
    (is (= "p" (#'parser/infix-to-postfix "p")))
    (is (= "pqvrs&=" (#'parser/infix-to-postfix "(pvq)=(r&s)")))
    (is (= "pqr&=" (#'parser/infix-to-postfix "p=(q&r)")))
    (is (= "pq&~" (#'parser/infix-to-postfix "~(p&q)")))))

(deftest postfix-to-statement-test
  (testing "does it work"
    (is (= (->Var \p) (#'parser/postfix-to-statement "p")))

    (is (= (->And (->Var \p)
                  (->Var \q)) (#'parser/postfix-to-statement "pq&")))

    (is (= (->Or (->Var \p)
                 (->Var \q)) (#'parser/postfix-to-statement "pqv")))

    (is (= (->Equ (->Var \p)
                  (->Var \q)) (#'parser/postfix-to-statement "pq=")))

    (is (= (->Ent (->Var \p)
                  (->Var \q)) (#'parser/postfix-to-statement "pq>")))

    (is (= (->Not (->And (->Var \p)
                         (->Var \q))) (#'parser/postfix-to-statement "pq&~")))

    (is (= (->Equ (->Or (->Var \p)
                        (->Var \q))
                  (->And (->Var \r)
                         (->Var \s))) (#'parser/postfix-to-statement "pqvrs&=")))))

(deftest string-to-statement-test
  (testing "does it work"
    (is (= (->Equ (->Or (->Var \p)
                        (->Var \q))
                  (->And (->Var \r)
                         (->Var \s))) (parser/string-to-statement "((pvq)=(r&s))")))))
