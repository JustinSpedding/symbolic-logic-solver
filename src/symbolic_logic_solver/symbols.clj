(ns symbolic-logic-solver.symbols
  (:require [symbolic-logic-solver.statements :refer :all]))

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

(defn char-to-record-constructor [op] ({and-operator ->And
                                        or-operator ->Or
                                        equivalent-operator ->Equ
                                        entails-operator ->Ent
                                        not-operator ->Not} op))
