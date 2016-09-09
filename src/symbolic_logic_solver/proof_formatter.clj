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

(defn indent-level [lines]
  (map #(update-in % [:level] inc) lines))

(defn AndElimination->lines [step]
  (conj (step->lines (:arg1 step))
        (->Line (statement->string (:conclusion step))
                (str and-operator "E %d")
                (list (->Reference (statement->string (:conclusion (:arg1 step))) nil))
                0)))

(defn OrElimination->lines [step]
  (conj (concat (indent-level (step->lines (:arg3 step)))
                (indent-level (step->lines (:arg2 step)))
                (step->lines (:arg1 step)))
        (->Line (statement->string (:conclusion step))
                (str or-operator "E %d, %d-%d, %d-%d")
                (list (->Reference (statement->string (:conclusion (:arg1 step))) nil)
                      (->Reference nil (statement->string (:assumption (:arg2 step))))
                      (->Reference (statement->string (:conclusion (:arg1 (:arg2 step)))) (statement->string (:assumption (:arg2 step))))
                      (->Reference nil (statement->string (:assumption (:arg3 step))))
                      (->Reference (statement->string (:conclusion (:arg1 (:arg3 step)))) (statement->string (:assumption (:arg3 step)))))
                0)))

(defn EquElimination->lines [step]
  (conj (concat (step->lines (:arg2 step))
                (step->lines (:arg1 step)))
        (->Line (statement->string (:conclusion step))
                (str equivalent-operator "E %d, %d")
                (list (->Reference (statement->string (:conclusion (:arg1 step))) nil)
                      (->Reference (statement->string (:conclusion (:arg2 step))) nil))
                0)))

(defn EntElimination->lines [step]
  (conj (concat (step->lines (:arg2 step))
                (step->lines (:arg1 step)))
        (->Line (statement->string (:conclusion step))
                (str entails-operator "E %d, %d")
                (list (->Reference (statement->string (:conclusion (:arg1 step))) nil)
                      (->Reference (statement->string (:conclusion (:arg2 step))) nil))
                0)))

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
