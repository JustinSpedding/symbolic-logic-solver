(ns symbolic-logic-solver.symbols)

(def open-paren \()

(def close-paren \))

(def and-operator \&)

(def or-operator \v)

(def equivalent-operator \=)

(def entails-operator \>)

(def not-operator \~)

(defn binary-operator? [op] (#{and-operator
                               or-operator
                               equivalent-operator
                               entails-operator} op))
