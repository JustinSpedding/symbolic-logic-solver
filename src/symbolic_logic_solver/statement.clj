(ns symbolic-logic-solver.statement
  (:require [symbolic-logic-solver.symbols :as symbols]))

(defn atom? [statement])

(defn- remove-whitespace [input-string]
  (remove #{\ } input-string))

(defn- pop-while-removed-item-makes-predicate-true [stack pred]
  (loop [stack stack]
    (if (empty? stack)
      stack
      (if (pred (peek stack))
        (recur (pop stack))
        stack))))

(defn- infix-to-postfix [input-string]
  (loop [remaining input-string
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

(defn- postfix-to-statement [postfix])

(defn string-to-statement [input-string]
  (-> input-string
      remove-whitespace
      infix-to-postfix
      postfix-to-statement))
