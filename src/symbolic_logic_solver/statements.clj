(ns symbolic-logic-solver.statements)

(defrecord Var [var])

(defrecord And [arg1 arg2])

(defrecord Or [arg1 arg2])

(defrecord Equ [arg1 arg2])

(defrecord Ent [arg1 arg2])

(defrecord Not [arg1])

(defn atom? [statement]
  (or (= Var (type statement))
      (and (= Not (type statement))
           (= Var (type (:arg1 statement))))))

(defn consistent? [& statements])
