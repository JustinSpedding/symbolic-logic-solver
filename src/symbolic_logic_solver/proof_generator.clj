(ns symbolic-logic-solver.proof-generator
  (:require [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all]))

(defn entails? [assumptions conclusion]
  (not (apply consistent? (conj assumptions (->Not conclusion)))))

(declare try-elimination)
(declare try-introduction)
(declare generate-proof)

(defn eliminate-var [assumptions last-step conclusion]
  (if (= (:conclusion last-step) conclusion)
    last-step))

(defn eliminate-and [assumptions last-step conclusion]
  (let [assumption-to-eliminate (:conclusion last-step)
        arg1 (:arg1 assumption-to-eliminate)
        arg2 (:arg2 assumption-to-eliminate)]
    (some #(if (= % conclusion)
             (->AndElimination last-step %))
          [arg1 arg2])))

;; TODO make this smarter so that other assumptions can be used inside
(defn eliminate-or [assumptions last-step conclusion]
  (let [assumption-to-eliminate (:conclusion last-step)
        arg1 (:arg1 assumption-to-eliminate)
        arg2 (:arg2 assumption-to-eliminate)]
    (if (and (entails? (list arg1) conclusion)
             (entails? (list arg2) conclusion))
      (->OrElimination last-step
                       (->Assumption arg1 (generate-proof (list arg1) conclusion))
                       (->Assumption arg2 (generate-proof (list arg2) conclusion))
                       conclusion))))

(defn eliminate-equ [assumptions last-step conclusion]
  (let [assumption-to-eliminate (:conclusion last-step)
        arg1 (:arg1 assumption-to-eliminate)
        arg2 (:arg2 assumption-to-eliminate)]
    (some #(if (and (= (first %) conclusion)
                    (entails? assumptions (second %)))
             (->EquElimination last-step
                               (generate-proof (remove (fn [x] (= assumption-to-eliminate x)) assumptions) (second %))
                               conclusion))
          [[arg1 arg2] [arg2 arg1]])))

(defn eliminate-ent [assumptions last-step conclusion]
  (let [assumption-to-eliminate (:conclusion last-step)
        arg1 (:arg1 assumption-to-eliminate)
        arg2 (:arg2 assumption-to-eliminate)]
    (if (and (= arg2 conclusion)
             (entails? assumptions arg1))
      (->EntElimination last-step
                        (generate-proof assumptions arg1)
                        conclusion))))

(defn eliminate-not [assumptions last-step conclusion]
  (let [assumption-to-eliminate (:conclusion last-step)
        arg1 (:arg1 assumption-to-eliminate)]
    (if (and (Not? arg1)
             (= (:arg1 arg1) conclusion))
      (->NotElimination last-step
                        conclusion))))

(defn introduce-and [assumptions conclusion])
(defn introduce-or  [assumptions conclusion])
(defn introduce-equ [assumptions conclusion])
(defn introduce-ent [assumptions conclusion])
(defn introduce-not [assumptions conclusion])
(defn introduce-any [assumptions conclusion])

;; TODO make this smarter so that it prioritizes some statement types over others
(defn try-elimination [assumptions conclusion]
  (some #(cond (Var? (:conclusion %)) (eliminate-var assumptions % conclusion)
               (And? (:conclusion %)) (eliminate-and assumptions % conclusion)
               (Or?  (:conclusion %)) (eliminate-or  assumptions % conclusion)
               (Equ? (:conclusion %)) (eliminate-equ assumptions % conclusion)
               (Ent? (:conclusion %)) (eliminate-ent assumptions % conclusion)
               (Not? (:conclusion %)) (eliminate-not assumptions % conclusion))
        (map ->Reiteration assumptions)))

(defn try-introduction [assumptions conclusion]
  (cond (And? conclusion) (introduce-and assumptions conclusion)
        (Or?  conclusion) (introduce-or  assumptions conclusion)
        (Equ? conclusion) (introduce-equ assumptions conclusion)
        (Ent? conclusion) (introduce-ent assumptions conclusion)
        (Not? conclusion) (introduce-not assumptions conclusion)))

(defn generate-proof [assumptions conclusion]
  (if (entails? assumptions conclusion)
    (or (try-elimination assumptions conclusion)
        (try-introduction assumptions conclusion)
        (introduce-any assumptions conclusion))))
