(ns symbolic-logic-solver.statement-converter
  (:require [symbolic-logic-solver.symbols :as symbols]
            [symbolic-logic-solver.statements :refer :all]))

(defn- remove-whitespace [input-string]
  (remove #{\ } input-string))

(defn- pop-while-removed-item-makes-predicate-true [stack pred]
  (loop [stack stack]
    (if (and (seq stack)
             (pred (peek stack)))
      (recur (pop stack))
      stack)))

(defn- infix-to-postfix [infix]
  (loop [remaining infix
         stack (list)
         output ""]
    (if-let [current-char (first remaining)]
      (cond (= symbols/open-paren current-char)     (recur (rest remaining)
                                                           (conj stack current-char)
                                                           output)
            (= symbols/close-paren current-char)    (recur (rest remaining)
                                                           (pop (pop-while-removed-item-makes-predicate-true stack #(not= symbols/open-paren %)))
                                                           (apply str output (take-while #(not= symbols/open-paren %) stack)))
            (symbols/binary-operator? current-char) (recur (rest remaining)
                                                           (conj (pop-while-removed-item-makes-predicate-true stack #(= symbols/not-operator %)) current-char)
                                                           (apply str output (take-while #(= symbols/not-operator %) stack)))
            (= symbols/not-operator current-char)   (recur (rest remaining)
                                                           (conj stack current-char)
                                                           output)
            :else                                   (recur (rest remaining)
                                                           stack
                                                           (str output current-char)))
      (apply str output stack))))

(defn- postfix-to-statement [postfix]
  (loop [remaining postfix
         stack (list)]
    (if-let [current-char (first remaining)]
      (cond (symbols/binary-operator? current-char) (recur (rest remaining)
                                                           (conj (drop 2 stack) ((symbols/char-to-record-constructor current-char) (second stack) (first stack))))
            (= symbols/not-operator current-char)   (recur (rest remaining)
                                                           (conj (drop 1 stack) (->Not (first stack))))
            :else                                   (recur (rest remaining)
                                                           (conj stack (->Var current-char))))
      (first stack))))

(defn string-to-statement [input-string]
  (-> input-string
      remove-whitespace
      infix-to-postfix
      postfix-to-statement))
