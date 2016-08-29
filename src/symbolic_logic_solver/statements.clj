(ns symbolic-logic-solver.statements
  (:require [clojure.set :use [instersection]]))

(defrecord Var [var])
(defrecord And [arg1 arg2])
(defrecord Or [arg1 arg2])
(defrecord Equ [arg1 arg2])
(defrecord Ent [arg1 arg2])
(defrecord Not [arg1])

(defn Var? [statement] (= Var (type statement)))
(defn And? [statement] (= And (type statement)))
(defn Or? [statement] (= Or (type statement)))
(defn Equ? [statement] (= Equ (type statement)))
(defn Ent? [statement] (= Ent (type statement)))
(defn Not? [statement] (= Not (type statement)))

(defn atom? [statement]
  (or (= Var (type statement))
      (and (= Not (type statement))
           (= Var (type (:arg1 statement))))))

(defn- consistent?-worker [statements true-vars false-vars]
  (loop [statements statements
         true-vars true-vars
         false-vars false-vars]
    (if (empty? (clojure.set/intersection true-vars false-vars))
      (if-let [statement (first statements)]
        (cond (Var? statement) (recur (rest statements) (conj true-vars statement) false-vars)
              (And? statement) (recur (conj (rest statements) (:arg1 statement) (:arg2 statement)) true-vars false-vars)
              (Or? statement)  (or (consistent?-worker (conj (rest statements) (:arg1 statement)) true-vars false-vars)
                                   (consistent?-worker (conj (rest statements) (:arg2 statement)) true-vars false-vars))
              (Equ? statement) (or (consistent?-worker (conj (rest statements) (:arg1 statement) (:arg2 statement)) true-vars false-vars)
                                   (consistent?-worker (conj (rest statements) (->Not (:arg1 statement)) (->Not (:arg2 statement))) true-vars false-vars))
              (Ent? statement) (or (consistent?-worker (conj (rest statements) (->Not (:arg1 statement))) true-vars false-vars)
                                   (consistent?-worker (conj (rest statements) (:arg2 statement)) true-vars false-vars))
              (Not? statement) (let [inner-statement (:arg1 statement)]
                                 (cond (Var? inner-statement) (recur (rest statements) true-vars (conj false-vars inner-statement))
                                       (And? inner-statement) (or (consistent?-worker (conj (rest statements) (->Not (:arg1 inner-statement))) true-vars false-vars)
                                                                  (consistent?-worker (conj (rest statements) (->Not (:arg2 inner-statement))) true-vars false-vars))
                                       (Or? inner-statement)  (recur (conj (rest statements) (->Not (:arg1 inner-statement)) (->Not (:arg2 inner-statement))) true-vars false-vars)
                                       (Equ? inner-statement) (or (consistent?-worker (conj (rest statements) (:arg1 inner-statement) (->Not (:arg2 inner-statement))) true-vars false-vars)
                                                                  (consistent?-worker (conj (rest statements) (->Not (:arg1 inner-statement)) (:arg2 inner-statement)) true-vars false-vars))
                                       (Ent? inner-statement) (recur (conj (rest statements) (:arg1 inner-statement) (->Not (:arg2 inner-statement))) true-vars false-vars)
                                       (Not? inner-statement) (recur (conj (rest statements) (:arg1 inner-statement)) true-vars false-vars))))
        true))))

(defn consistent? [& statements]
  (consistent?-worker statements #{} #{}))
