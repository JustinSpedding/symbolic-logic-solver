(ns symbolic-logic-solver.steps)

(defrecord Reiteration [conclusion])

(defrecord AndElimination [arg1 conclusion])
(defrecord OrElimination [arg1 arg2 arg3 conclusion])
(defrecord EquElimination [arg1 arg2 conclusion])
(defrecord EntElimination [arg1 arg2 conclusion])
(defrecord NotElimination [arg1 conclusion])

(defrecord AndIntroduction [arg1 arg2 conclusion])
(defrecord OrIntroduction [arg1 conclusion])
(defrecord EquIntroduction [arg1 arg2 conclusion])
(defrecord EntIntroduction [arg1 conclusion])
(defrecord NotIntroduction [arg1 conclusion])
