(ns symbolic-logic-solver.proof-generator
  (:require [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all]))

(defn entails? [assumptions conclusion]
  (not (apply consistent? (conj assumptions (->Not conclusion)))))

(declare generate-proof)

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

(defn introduce-and [assumptions conclusion]
  (if (and (entails? assumptions (:arg1 conclusion))
           (entails? assumptions (:arg2 conclusion)))
    (->AndIntroduction (generate-proof assumptions (:arg1 conclusion))
                       (generate-proof assumptions (:arg2 conclusion))
                       conclusion)))

(defn introduce-or  [assumptions conclusion]
  (some #(if (entails? assumptions %)
           (->OrIntroduction (generate-proof assumptions %) conclusion))
        [(:arg1 conclusion) (:arg2 conclusion)]))

(defn introduce-equ [assumptions conclusion])
(defn introduce-ent [assumptions conclusion])
(defn introduce-not [assumptions conclusion])

(defn try-reiteration [assumptions conclusion]
  (some #(if (= % conclusion)
           (->Reiteration conclusion))
        assumptions))

;; TODO make this smarter so that it prioritizes some statement types over others
(defn try-elimination [assumptions conclusion]
  (some #(cond (And? %) (eliminate-and assumptions (->Reiteration %) conclusion)
               (Or?  %) (eliminate-or  assumptions (->Reiteration %) conclusion)
               (Equ? %) (eliminate-equ assumptions (->Reiteration %) conclusion)
               (Ent? %) (eliminate-ent assumptions (->Reiteration %) conclusion)
               (Not? %) (eliminate-not assumptions (->Reiteration %) conclusion))
        assumptions))

(defn try-introduction [assumptions conclusion]
  (cond (And? conclusion) (introduce-and assumptions conclusion)
        (Or?  conclusion) (introduce-or  assumptions conclusion)
        (Equ? conclusion) (introduce-equ assumptions conclusion)
        (Ent? conclusion) (introduce-ent assumptions conclusion)
        (Not? conclusion) (introduce-not assumptions conclusion)))

(defn indirect-proof [assumptions conclusion])

(defn generate-proof [assumptions conclusion]
  (if (entails? assumptions conclusion)
    (or (try-reiteration assumptions conclusion)
        (try-elimination assumptions conclusion)
        (try-introduction assumptions conclusion)
        (indirect-proof assumptions conclusion))))
