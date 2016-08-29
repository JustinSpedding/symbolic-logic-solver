(ns symbolic-logic-solver.statement-converter-test
  (:require [clojure.test :refer :all]
            [symbolic-logic-solver.statement-converter :as statement]
            [symbolic-logic-solver.statements :refer :all]))

(deftest remove-whitespace-test
  (testing "removes all spaces and keeps non-whitespace chars"
    (is (= (seq "helloworld") (#'statement/remove-whitespace "   hello   world   ")))))

(deftest infix-to-postfix-test
  (testing "does it work"
    (is (= "ppv" (#'statement/infix-to-postfix "pvp")))
    (is (= "pq&" (#'statement/infix-to-postfix "p&q")))
    (is (= "pq>" (#'statement/infix-to-postfix "p>q")))
    (is (= "pq=" (#'statement/infix-to-postfix "p=q")))
    (is (= "ppv" (#'statement/infix-to-postfix "(pvp)")))
    (is (= "p~" (#'statement/infix-to-postfix "~p")))
    (is (= "p~p~v" (#'statement/infix-to-postfix "~pv~p")))
    (is (= "p~~" (#'statement/infix-to-postfix "(~~p)")))
    (is (= "p" (#'statement/infix-to-postfix "p")))
    (is (= "pqvrs&=" (#'statement/infix-to-postfix "(pvq)=(r&s)")))
    (is (= "pqr&=" (#'statement/infix-to-postfix "p=(q&r)")))
    (is (= "pq&~" (#'statement/infix-to-postfix "~(p&q)")))))

(deftest postfix-to-statement-test
  (testing "does it work"
    (is (= (->Var \p) (#'statement/postfix-to-statement "p")))

    (is (= (->And (->Var \p)
                  (->Var \q)) (#'statement/postfix-to-statement "pq&")))

    (is (= (->Or (->Var \p)
                 (->Var \q)) (#'statement/postfix-to-statement "pqv")))

    (is (= (->Equ (->Var \p)
                  (->Var \q)) (#'statement/postfix-to-statement "pq=")))

    (is (= (->Ent (->Var \p)
                  (->Var \q)) (#'statement/postfix-to-statement "pq>")))

    (is (= (->Not (->And (->Var \p)
                         (->Var \q))) (#'statement/postfix-to-statement "pq&~")))

    (is (= (->Equ (->Or (->Var \p)
                        (->Var \q))
                  (->And (->Var \r)
                         (->Var \s))) (#'statement/postfix-to-statement "pqvrs&=")))))

(deftest string-to-statement-test
  (testing "does it work"
    (is (= (->Equ (->Or (->Var \p)
                        (->Var \q))
                  (->And (->Var \r)
                         (->Var \s))) (#'statement/string-to-statement "((pvq)=(r&s))")))))
