(ns symbolic-logic-solver.proof-formatter
  (:require [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all]
            [symbolic-logic-solver.symbols :refer :all]))

(defrecord Line [statement step-type references indentation])
(defrecord AssumptionLine [statement indentation])

(defrecord StatementReference [statement])
(defrecord ReiterationReference [statement])
(defrecord AssumptionReference [statement])
(defrecord InnerAssumptionReference [statement assumption])

(defn statement->string [x]
  (if-let [op (statement->operator x)]
    (if (binary-operator? op)
      (str "(" (statement->string (:arg1 x)) (statement->operator x) (statement->string (:arg2 x)) ")")
      (str not-operator (statement->string (:arg1 x))))
    (str (:var x))))

(declare step->lines)

(defn indent [lines]
  (map #(update-in % [:indentation] inc) lines))

(defmacro defn-line-converter [fn-name reference-string & args-to-reference]
  (list 'defn fn-name ['step]
    (list 'conj (apply list 'concat (map #(case (first %)
                                            :statement (list 'step->lines (list (second %) 'step))
                                            :assumption (list 'indent (list 'step->lines (list (second %) 'step)))
                                            :contradiction (list 'indent (list 'step->lines (list (second %) 'step))))
                                         (reverse args-to-reference)))
      (list '->Line (list 'statement->string (list ':conclusion 'step))
                    reference-string
                    (apply list 'list (mapcat #(case (first %)
                                                 :statement (list (list '->StatementReference (list 'statement->string (list ':conclusion (list (second %) 'step)))))
                                                 :assumption (list (list '->AssumptionReference (list 'statement->string (list ':assumption (list (second %) 'step))))
                                                                   (list '->InnerAssumptionReference (list 'statement->string (list ':conclusion (list ':arg1 (list (second %) 'step)))) (list 'statement->string (list ':assumption (list (second %) 'step)))))
                                                 :contradiction (list (list '->AssumptionReference (list 'statement->string (list ':assumption (list (second %) 'step))))
                                                                      (list '->InnerAssumptionReference (list 'statement->string (list ':conclusion (list ':arg1 (list (second %) 'step)))) (list 'statement->string (list ':assumption (list (second %) 'step))))
                                                                      (list '->InnerAssumptionReference (list 'statement->string (list ':conclusion (list ':arg2 (list (second %) 'step)))) (list 'statement->string (list ':assumption (list (second %) 'step))))))
                                              args-to-reference))
                    '0))))

(defn-line-converter AndElimination->lines (str and-operator "E %d") [:statement :arg1])
(defn-line-converter OrElimination->lines  (str or-operator "E %d, %d-%d, %d-%d") [:statement :arg1] [:assumption :arg2] [:assumption :arg3])
(defn-line-converter EquElimination->lines (str equivalent-operator "E %d, %d") [:statement :arg1] [:statement :arg2])
(defn-line-converter EntElimination->lines (str entails-operator "E %d, %d") [:statement :arg1] [:statement :arg2])
(defn-line-converter NotElimination->lines (str not-operator "E %d") [:statement :arg1])

(defn-line-converter AndIntroduction->lines (str and-operator "I %d, %d") [:statement :arg1] [:statement :arg2])
(defn-line-converter OrIntroduction->lines  (str or-operator "I %d") [:statement :arg1])
(defn-line-converter EquIntroduction->lines (str equivalent-operator "I %d-%d, %d-%d") [:assumption :arg1] [:assumption :arg2])
(defn-line-converter EntIntroduction->lines (str entails-operator "I %d-%d") [:assumption :arg1])
(defn-line-converter NotIntroduction->lines (str not-operator "I %d(%d, %d)") [:contradiction :arg1])

(defn Reiteration->lines [step]
  (list (->Line (statement->string (:conclusion step))
                "R %d"
                (list (->ReiterationReference (statement->string (:conclusion step))))
                0)))

(defn Assumption->lines [step]
  (concat (step->lines (:arg1 step))
          (list (->AssumptionLine (statement->string (:assumption step)) 0))))

(defn Contradiction->lines [step]
  (concat (step->lines (:arg2 step))
          (step->lines (:arg1 step))
          (list (->AssumptionLine (statement->string (:assumption step)) 0))))

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

(defn find-referenced-statement [reference indexed-lines current-line]
  (cond (= StatementReference (type reference)) 0
        (= ReiterationReference (type reference)) 0
        (= AssumptionReference (type reference)) 0
        (= InnerAssumptionReference (type reference)) 0))

(defn line->string [indexed-line indexed-lines]
  (let [index (first indexed-line)
        line (second indexed-line)
        indents (clojure.string/join (repeat (:indentation line) "|"))]
    (cond (= Line (type line)) (str (format "%3d" index) ": |" indents (:statement line) " " (apply format (:step-type line) (map #(find-referenced-statement % indexed-lines indexed-line) (:references line))))
          (= AssumptionLine (type line)) (str (format "%3d" index) ": |" indents (:statement line) \newline "     |" indents "----------------"))))

(defn format-proof [assumptions proof]
  (let [indexed-lines (->> proof
                           step->lines
                           reverse
                           (concat (map #(->AssumptionLine (statement->string %) 0) assumptions))
                           (map-indexed vector)
                           vec)]
    (clojure.string/join \newline (map #(line->string % indexed-lines) indexed-lines))))
