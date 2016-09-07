(ns symbolic-logic-solver.steps)

(defrecord AndElimination [arg1 conclusion])
(defrecord OrElimination  [arg1 arg2 arg3 conclusion])
(defrecord EquElimination [arg1 arg2 conclusion])
(defrecord EntElimination [arg1 arg2 conclusion])
(defrecord NotElimination [arg1 conclusion])

(defrecord AndIntroduction [arg1 arg2 conclusion])
(defrecord OrIntroduction  [arg1 conclusion])
(defrecord EquIntroduction [arg1 arg2 conclusion])
(defrecord EntIntroduction [arg1 conclusion])
(defrecord NotIntroduction [arg1 conclusion])

(defrecord Reiteration [conclusion])
(defrecord Assumption [assumption arg1])
(defrecord Contradiction [assumption arg1 arg2])

(defn AndElimination? [step] (= AndElimination (type step)))
(defn OrElimination?  [step] (= OrElimination  (type step)))
(defn EquElimination? [step] (= EquElimination (type step)))
(defn EntElimination? [step] (= EntElimination (type step)))
(defn NotElimination? [step] (= NotElimination (type step)))

(defn AndIntroduction? [step] (= AndIntroduction (type step)))
(defn OrIntroduction?  [step] (= OrIntroduction  (type step)))
(defn EquIntroduction? [step] (= EquIntroduction (type step)))
(defn EntIntroduction? [step] (= EntIntroduction (type step)))
(defn NotIntroduction? [step] (= NotIntroduction (type step)))

(defn Reiteration? [step] (= Reiteration (type step)))
(defn Assumption? [step] (= Assumption (type step)))
(defn Contradiction? [step] (= Contradiction (type step)))
