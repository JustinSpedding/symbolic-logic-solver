(ns symbolic-logic-solver.proof-formatter
  (:require [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all]
            [symbolic-logic-solver.symbols :refer :all]))

(defrecord Line [statement step-type references level])
(defrecord AssumptionLine [assumption level])
(defrecord Reference [statement assumption])

(defn statement->string [x]
  (if-let [op (statement->operator x)]
    (if (binary-operator? op)
      (str "(" (statement->string (:arg1 x)) (statement->operator x) (statement->string (:arg2 x)) ")")
      (str not-operator (statement->string (:arg1 x))))
    (str (:var x))))

(declare step->lines)

(defn AndElimination->lines [step]
  (cons (->Line (statement->string (:conclusion step))
                (str and-operator "E %d")
                (list (->Reference (:conclusion (:arg1 step) nil)))
                0)
        (step->lines (:arg1 step))))

(defn OrElimination->lines [step])
(defn EquElimination->lines [step])
(defn EntElimination->lines [step])
(defn NotElimination->lines [step])
(defn AndIntroduction->lines [step])
(defn OrIntroduction->lines [step])
(defn EquIntroduction->lines [step])
(defn EntIntroduction->lines [step])
(defn NotIntroduction->lines [step])
(defn Reiteration->lines [step])
(defn Assumption->lines [step])
(defn Contradiction->lines [step])

(defn step->lines [step]
  (cond (AndElimination? step) (AndElimination->lines step)
        (OrElimination? step) (OrElimination->lines step)
        (EquElimination? step) (EquElimination->lines step)
        (EntElimination? step) (EntElimination->lines step)
        (NotElimination? step) (NotElimination->lines step)
        (AndIntroduction? step) (AndIntroduction->lines step)
        (OrIntroduction? step) (OrIntroduction->lines step)
        (EquIntroduction? step) (EquIntroduction->lines step)
        (EntIntroduction? step) (EntIntroduction->lines step)
        (NotIntroduction? step) (NotIntroduction->lines step)
        (Reiteration? step) (Reiteration->lines step)
        (Assumption? step) (Assumption->lines step)
        (Contradiction? step) (Contradiction->lines step)))

(defn format-proof [assumptions proof])
