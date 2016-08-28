(ns symbolic-logic-solver.statements)

(defrecord Var [var])

(defrecord And [arg1 arg2])

(defrecord Or [arg1 arg2])

(defrecord Equ [arg1 arg2])

(defrecord Ent [arg1 arg2])

(defrecord Not [arg1])
