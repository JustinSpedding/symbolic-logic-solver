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

(deftest postfix-to-statement-test
  (testing "does it work"
    (is (= (list :var \p) (#'statement/postfix-to-statement "p")))
    (is (= (list :and (list :var \p) (list :var \q)) (#'statement/postfix-to-statement "pq&")))
    (is (= (list :or (list :var \p) (list :var \q)) (#'statement/postfix-to-statement "pqv")))
    (is (= (list :equ (list :var \p) (list :var \q)) (#'statement/postfix-to-statement "pq=")))
    (is (= (list :ent (list :var \p) (list :var \q)) (#'statement/postfix-to-statement "pq>")))
    (is (= (list :not (list :and (list :var \p) (list :var \q))) (#'statement/postfix-to-statement "pq&~")))
    (is (= (list :equ (list :or (list :var \p) (list :var \q)) (list :and (list :var \r) (list :var \s))) (#'statement/postfix-to-statement "pqvrs&=")))))

(deftest string-to-statement-test)
