(ns symbolic-logic-solver.proof-generator
  (:require [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all])
  (:import [symbolic_logic_solver.statements Var And Or Equ Ent Not]))

(defn entails? [assumptions conclusion]
  (not (apply consistent? (conj assumptions (->Not conclusion)))))

(declare generate-proof)

(defmulti eliminate (fn [assumptions step conclusion] (class (:conclusion step))))

(defmethod eliminate And [assumptions last-step conclusion]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (some #(if (and (entails? (list %) conclusion)
                    (not (entails? (list) conclusion)))
             (if (= conclusion %)
               (->AndElimination last-step %)
               (eliminate assumptions (->AndElimination last-step %) conclusion)))
          [arg1 arg2])))

;; TODO make this smarter so that other assumptions can be used inside
(defmethod eliminate Or [assumptions last-step conclusion]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (if (and (entails? (list arg1) conclusion)
             (entails? (list arg2) conclusion))
      (->OrElimination last-step
                       (->Assumption arg1 (generate-proof (list arg1) conclusion))
                       (->Assumption arg2 (generate-proof (list arg2) conclusion))
                       conclusion))))

(defmethod eliminate Equ [assumptions last-step conclusion]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (some #(if (and (= (first %) conclusion)
                    (entails? assumptions (second %)))
             (->EquElimination last-step
                               (generate-proof (remove (fn [x] (= statement-to-eliminate x)) assumptions) (second %))
                               conclusion))
          [[arg1 arg2] [arg2 arg1]])))

(defmethod eliminate Ent [assumptions last-step conclusion]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (if (and (= arg2 conclusion)
             (entails? assumptions arg1))
      (->EntElimination last-step
                        (generate-proof assumptions arg1)
                        conclusion))))

(defmethod eliminate Not [assumptions last-step conclusion]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)]
    (if (and (Not? arg1)
             (= (:arg1 arg1) conclusion))
      (->NotElimination last-step
                        conclusion))))

(defn introduce-and [assumptions conclusion]
  (if (and (entails? assumptions (:arg1 conclusion))
           (entails? assumptions (:arg2 conclusion)))
    (->AndIntroduction (generate-proof assumptions (:arg1 conclusion))
                       (generate-proof assumptions (:arg2 conclusion))
                       conclusion)))

(defn introduce-or [assumptions conclusion]
  (some #(if (entails? assumptions %)
           (->OrIntroduction (generate-proof assumptions %) conclusion))
        [(:arg1 conclusion) (:arg2 conclusion)]))

(defn introduce-equ [assumptions conclusion]
  (let [arg1 (:arg1 conclusion)
        arg2 (:arg2 conclusion)]
    (if (and (entails? (conj assumptions arg1) arg2)
             (entails? (conj assumptions arg2) arg1))
      (->EquIntroduction (->Assumption arg1 (generate-proof (conj assumptions arg1) arg2))
                         (->Assumption arg2 (generate-proof (conj assumptions arg2) arg1))
                         conclusion))))

(defn introduce-ent [assumptions conclusion]
  (let [arg1 (:arg1 conclusion)
        arg2 (:arg2 conclusion)]
    (if (entails? (conj assumptions arg1) arg2)
      (->EntIntroduction (->Assumption arg1 (generate-proof (conj assumptions arg1) arg2))
                         conclusion))))

(defn introduce-not [assumptions conclusion]
  (let [new-assumptions (conj assumptions (:arg1 conclusion))]
    (if-let [contradiction (find-contradiction new-assumptions)]
      (->NotIntroduction (->Contradiction (:arg1 conclusion)
                                          (generate-proof (conj assumptions (:arg1 conclusion)) contradiction)
                                          (generate-proof (conj assumptions (:arg1 conclusion)) (->Not contradiction)))
                         conclusion))))

(defn try-reiteration [assumptions conclusion]
  (some #(if (= % conclusion)
           (->Reiteration conclusion))
        assumptions))

(defn try-elimination [assumptions conclusion]
  (some #(eliminate assumptions (->Reiteration %) conclusion)
        (sort-by statement-priority assumptions)))

(defn try-introduction [assumptions conclusion]
  (cond (And? conclusion) (introduce-and assumptions conclusion)
        (Or?  conclusion) (introduce-or  assumptions conclusion)
        (Equ? conclusion) (introduce-equ assumptions conclusion)
        (Ent? conclusion) (introduce-ent assumptions conclusion)
        (Not? conclusion) (introduce-not assumptions conclusion)))

(defn indirect-proof [assumptions conclusion]
  (if-let [proof (introduce-not assumptions (->Not (->Not conclusion)))]
    (->NotElimination proof conclusion)))

;; TODO do not check entails? within this function
(defn generate-proof [assumptions conclusion]
  (if (entails? assumptions conclusion)
    (or (try-reiteration assumptions conclusion)
        (try-elimination assumptions conclusion)
        (try-introduction assumptions conclusion)
        (indirect-proof assumptions conclusion))))
