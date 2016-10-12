(ns symbolic-logic-solver.proof-generator
  (:require [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all])
  (:import [symbolic_logic_solver.statements Var And Or Equ Ent Not]))

(defn entails? [assumptions conclusion]
  (not (apply consistent? (conj assumptions (->Not conclusion)))))

(declare generate-proof)

(defmulti eliminate-multi (fn [assumptions last-step conclusion already-eliminated] (class (:conclusion last-step))))

(defn eliminate [assumptions last-step conclusion already-eliminated]
  (cond (= conclusion (:conclusion last-step)) last-step
        (entails? (list) conclusion) nil
        :else (eliminate-multi assumptions last-step conclusion already-eliminated)))

(defmethod eliminate-multi Var [assumptions last-step conclusion already-eliminated] nil)

(defmethod eliminate-multi And [assumptions last-step conclusion already-eliminated]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (some #(if (and (not (already-eliminated %)))
             (eliminate assumptions (->AndElimination last-step %) conclusion (conj already-eliminated %)))
          [arg1 arg2])))

;; TODO make this smarter so that other assumptions can be used inside
(defmethod eliminate-multi Or [assumptions last-step conclusion already-eliminated]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (if (and (entails? (list arg1) conclusion)
             (entails? (list arg2) conclusion))
      (->OrElimination last-step
                       (->Assumption arg1 (generate-proof (list arg1) conclusion))
                       (->Assumption arg2 (generate-proof (list arg2) conclusion))
                       conclusion))))

(defmethod eliminate-multi Equ [assumptions last-step conclusion already-eliminated]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (some #(if (and (not (already-eliminated (first %)))
                    (entails? (list (first %)) conclusion)
                    (entails? assumptions (second %)))
             (eliminate assumptions
                        (->EquElimination last-step
                                          (generate-proof (remove (fn [x] (= statement-to-eliminate x)) assumptions) (second %))
                                          (first %))
                        conclusion
                        (conj already-eliminated (first %))))
          [[arg1 arg2] [arg2 arg1]])))

(defmethod eliminate-multi Ent [assumptions last-step conclusion already-eliminated]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (if (and (not (already-eliminated arg2))
             (entails? assumptions arg1))
      (eliminate assumptions
                 (->EntElimination last-step
                                   (generate-proof assumptions arg1)
                                   arg2)
                 conclusion
                 (conj already-eliminated arg2)))))

(defmethod eliminate-multi Not [assumptions last-step conclusion already-eliminated]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)]
    (if (and (Not? arg1)
             (not (already-eliminated (:arg1 arg1)))
             (entails? (list (:arg1 arg1)) conclusion))
      (eliminate assumptions
                 (->NotElimination last-step
                                   (:arg1 arg1))
                 conclusion
                 (conj already-eliminated (:arg1 arg1))))))

(defmulti introduce (fn [assumptions conclusion] (class conclusion)))

(defmethod introduce Var [assumptions conclusion] nil)

(defmethod introduce And [assumptions conclusion]
  (if (and (entails? assumptions (:arg1 conclusion))
           (entails? assumptions (:arg2 conclusion)))
    (->AndIntroduction (generate-proof assumptions (:arg1 conclusion))
                       (generate-proof assumptions (:arg2 conclusion))
                       conclusion)))

(defmethod introduce Or [assumptions conclusion]
  (some #(if (entails? assumptions %)
           (->OrIntroduction (generate-proof assumptions %) conclusion))
        [(:arg1 conclusion) (:arg2 conclusion)]))

(defmethod introduce Equ [assumptions conclusion]
  (let [arg1 (:arg1 conclusion)
        arg2 (:arg2 conclusion)]
    (if (and (entails? (conj assumptions arg1) arg2)
             (entails? (conj assumptions arg2) arg1))
      (->EquIntroduction (->Assumption arg1 (generate-proof (conj assumptions arg1) arg2))
                         (->Assumption arg2 (generate-proof (conj assumptions arg2) arg1))
                         conclusion))))

(defmethod introduce Ent [assumptions conclusion]
  (let [arg1 (:arg1 conclusion)
        arg2 (:arg2 conclusion)]
    (if (entails? (conj assumptions arg1) arg2)
      (->EntIntroduction (->Assumption arg1 (generate-proof (conj assumptions arg1) arg2))
                         conclusion))))

(defmethod introduce Not [assumptions conclusion]
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
  (some #(eliminate assumptions (->Reiteration %) conclusion (hash-set))
        (sort-by statement-priority assumptions)))

(defn try-introduction [assumptions conclusion]
  (introduce assumptions conclusion))

(defn indirect-proof [assumptions conclusion]
  (if-let [proof (introduce assumptions (->Not (->Not conclusion)))]
    (->NotElimination proof conclusion)))

;; TODO do not check entails? within this function
(defn generate-proof [assumptions conclusion]
  (if (entails? assumptions conclusion)
    (or (try-reiteration assumptions conclusion)
        (try-elimination assumptions conclusion)
        (try-introduction assumptions conclusion)
        (indirect-proof assumptions conclusion))))
