(ns symbolic-logic-solver.statement-test
  (:require [clojure.test :refer :all]
            [symbolic-logic-solver.statement :as statement]))

(deftest atom?-test)

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

(deftest string-to-statement-test)
