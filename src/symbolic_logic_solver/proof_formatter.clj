(ns symbolic-logic-solver.proof-formatter
  (:require [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all]
            [symbolic-logic-solver.symbols :refer :all]))

(defrecord Line [statement step-type references indentation])
(defrecord AssumptionLine [assumption indentation])

(defrecord Reference [statement assumption])

(defn statement->string [x]
  (if-let [op (statement->operator x)]
    (if (binary-operator? op)
      (str "(" (statement->string (:arg1 x)) (statement->operator x) (statement->string (:arg2 x)) ")")
      (str not-operator (statement->string (:arg1 x))))
    (str (:var x))))

(declare step->lines)

(defn indent [lines]
  (map #(update-in % [:indentation] inc) lines))

(defmacro defn-line-converter [fn-name reference-string args-to-reference]
  (list 'defn fn-name ['step]
    (list 'conj (apply list 'concat (map #(case (first %)
                                            :statement (list 'step->lines (list (second %) 'step))
                                            :assumption (list 'indent (list 'step->lines (list (second %) 'step)))
                                            :contradiction (list 'indent (list 'step->lines (list (second %) 'step)))) (reverse args-to-reference)))
      (list '->Line (list 'statement->string (list ':conclusion 'step))
                    reference-string
                    (apply list 'list (mapcat #(case (first %)
                                              :statement (list (list '->Reference (list 'statement->string (list ':conclusion (list (second %) 'step))) 'nil))
                                              :assumption (list (list '->Reference 'nil (list 'statement->string (list ':assumption (list (second %) 'step))))
                                                                (list '->Reference (list 'statement->string (list ':conclusion (list ':arg1 (list (second %) 'step)))) (list 'statement->string (list ':assumption (list (second %) 'step)))))
                                              :contradiction (list (list '->Reference 'nil (list 'statement->string (list ':assumption (list (second %) 'step))))
                                                                   (list '->Reference (list 'statement->string (list ':conclusion (list ':arg1 (list (second %) 'step)))) (list 'statement->string (list ':assumption (list (second %) 'step))))
                                                                   (list '->Reference (list 'statement->string (list ':conclusion (list ':arg2 (list (second %) 'step)))) (list 'statement->string (list ':assumption (list (second %) 'step))))))
                                           args-to-reference))
                    '0))))

(defn-line-converter AndElimination->lines (str and-operator "E %d") [[:statement :arg1]])

(defn-line-converter OrElimination->lines (str or-operator "E %d, %d-%d, %d-%d") [[:statement :arg1] [:assumption :arg2] [:assumption :arg3]])

(defn-line-converter EquElimination->lines (str equivalent-operator "E %d, %d") [[:statement :arg1] [:statement :arg2]])

(defn-line-converter EntElimination->lines (str entails-operator "E %d, %d") [[:statement :arg1] [:statement :arg2]])

(defn-line-converter NotElimination->lines (str not-operator "E %d") [[:statement :arg1]])

(defn-line-converter AndIntroduction->lines (str and-operator "I %d, %d") [[:statement :arg1] [:statement :arg2]])

(defn-line-converter OrIntroduction->lines (str or-operator "I %d") [[:statement :arg1]])

(defn-line-converter EquIntroduction->lines (str equivalent-operator "I %d-%d, %d-%d") [[:assumption :arg1] [:assumption :arg2]])

(defn-line-converter EntIntroduction->lines (str entails-operator "I %d-%d") [[:assumption :arg1]])

(defn-line-converter NotIntroduction->lines (str not-operator "I %d(%d, %d)") [[:contradiction :arg1]])

(defn Reiteration->lines [step]
  (list (->Line (statement->string (:conclusion step))
                "R %d"
                (list (->Reference nil (statement->string (:conclusion step))))
                0)))

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
