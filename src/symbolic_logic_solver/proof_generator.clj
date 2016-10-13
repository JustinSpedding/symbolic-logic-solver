(ns symbolic-logic-solver.proof-generator
  (:require [symbolic-logic-solver.statements :refer :all]
            [symbolic-logic-solver.steps :refer :all])
  (:import [symbolic_logic_solver.statements Var And Or Equ Ent Not]))

(defmacro if-let*
  ([bindings then]
   `(if-let* ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(if-let [~(first bindings) ~(second bindings)]
        (if-let* ~(drop 2 bindings) ~then ~else)
        ~(if-not (second bindings) else))
     then)))

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
      (if-let* [inner-proof1 (generate-proof (list arg1) conclusion)
                inner-proof2 (generate-proof (list arg2) conclusion)]
        (->OrElimination last-step (->Assumption arg1 inner-proof1) (->Assumption arg2 inner-proof2) conclusion)))))

(defmethod eliminate-multi Equ [assumptions last-step conclusion already-eliminated]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (some #(if (and (not (already-eliminated (first %)))
                    (entails? assumptions (second %)))
             (if-let [inner-proof (generate-proof assumptions (second %) (conj already-eliminated (first %)))]
               (eliminate assumptions
                          (->EquElimination last-step inner-proof (first %))
                          conclusion
                          (conj already-eliminated (first %)))))
          [[arg1 arg2] [arg2 arg1]])))

(defmethod eliminate-multi Ent [assumptions last-step conclusion already-eliminated]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)
        arg2 (:arg2 statement-to-eliminate)]
    (if (and (not (already-eliminated arg2))
             (entails? assumptions arg1))
      (if-let [inner-proof (generate-proof assumptions arg1 (conj already-eliminated arg2))]
        (eliminate assumptions
                   (->EntElimination last-step inner-proof arg2)
                   conclusion
                   (conj already-eliminated arg2))))))

(defmulti eliminate-not-multi (fn [assumptions last-step conclusion already-eliminated] (class (:arg1 (:conclusion last-step)))))

(defmethod eliminate-multi Not [assumptions last-step conclusion already-eliminated]
  (eliminate-not-multi assumptions last-step conclusion already-eliminated))

(defmethod eliminate-not-multi Var [assumptions last-step conclusion already-eliminated] nil)

(defmethod eliminate-not-multi And [assumptions last-step conclusion already-eliminated] (throw (UnsupportedOperationException.)))

(defmethod eliminate-not-multi Or [assumptions last-step conclusion already-eliminated] (throw (UnsupportedOperationException.)))

(defmethod eliminate-not-multi Equ [assumptions last-step conclusion already-eliminated] (throw (UnsupportedOperationException.)))

(defmethod eliminate-not-multi Ent [assumptions last-step conclusion already-eliminated] (throw (UnsupportedOperationException.)))

(defmethod eliminate-not-multi Not [assumptions last-step conclusion already-eliminated]
  (let [statement-to-eliminate (:conclusion last-step)
        arg1 (:arg1 statement-to-eliminate)]
    (if (and (not (already-eliminated (:arg1 arg1))))
      (eliminate assumptions
                 (->NotElimination last-step (:arg1 arg1))
                 conclusion
                 (conj already-eliminated (:arg1 arg1))))))

(defmulti introduce (fn [assumptions conclusion] (class conclusion)))

(defmethod introduce Var [assumptions conclusion] nil)

(defmethod introduce And [assumptions conclusion]
  (if (and (entails? assumptions (:arg1 conclusion))
           (entails? assumptions (:arg2 conclusion)))
    (if-let* [inner-proof1 (generate-proof assumptions (:arg1 conclusion))
              inner-proof2 (generate-proof assumptions (:arg2 conclusion))]
      (->AndIntroduction inner-proof1 inner-proof2 conclusion))))

(defmethod introduce Or [assumptions conclusion]
  (some #(if (entails? assumptions %)
           (if-let [inner-proof (generate-proof assumptions %)]
             (->OrIntroduction inner-proof conclusion)))
        [(:arg1 conclusion) (:arg2 conclusion)]))

(defmethod introduce Equ [assumptions conclusion]
  (let [arg1 (:arg1 conclusion)
        arg2 (:arg2 conclusion)]
    (if (and (entails? (conj assumptions arg1) arg2)
             (entails? (conj assumptions arg2) arg1))
      (if-let* [inner-proof1 (generate-proof (conj assumptions arg1) arg2)
                inner-proof2 (generate-proof (conj assumptions arg2) arg1)]
        (->EquIntroduction (->Assumption arg1 inner-proof1) (->Assumption arg2 inner-proof2) conclusion)))))

(defmethod introduce Ent [assumptions conclusion]
  (let [arg1 (:arg1 conclusion)
        arg2 (:arg2 conclusion)]
    (if (entails? (conj assumptions arg1) arg2)
      (if-let [inner-proof (generate-proof (conj assumptions arg1) arg2)]
        (->EntIntroduction (->Assumption arg1 inner-proof) conclusion)))))

(defmethod introduce Not [assumptions conclusion]
  (if (not-any? #{(:arg1 conclusion)} assumptions)
    (let [new-assumptions (conj assumptions (:arg1 conclusion))]
      (if-let* [contradiction (find-contradiction new-assumptions)
                inner-proof1 (generate-proof (conj assumptions (:arg1 conclusion)) contradiction)
                inner-proof2 (generate-proof (conj assumptions (:arg1 conclusion)) (->Not contradiction))]
        (->NotIntroduction (->Contradiction (:arg1 conclusion) inner-proof1 inner-proof2) conclusion)))))

(defn try-reiteration [assumptions conclusion]
  (some #(if (= % conclusion)
           (->Reiteration conclusion))
        assumptions))

(defn try-elimination [assumptions conclusion already-eliminated]
  (some #(eliminate assumptions (->Reiteration %) conclusion already-eliminated)
        (sort-by statement-priority assumptions)))

(defn try-introduction [assumptions conclusion]
  (introduce assumptions conclusion))

(defn indirect-proof [assumptions conclusion]
  (if (not= Not (class conclusion)) ; If not introduction already failed earlier, do not try again
    (if-let [proof (introduce assumptions (->Not (->Not conclusion)))]
      (->NotElimination proof conclusion))))

;; TODO do not check entails? within this function
(defn generate-proof
  ([assumptions conclusion]
   (generate-proof assumptions conclusion (hash-set)))

  ([assumptions conclusion already-eliminated]
   (if (entails? assumptions conclusion)
     (or (try-reiteration assumptions conclusion)
         (try-elimination assumptions conclusion already-eliminated)
         (try-introduction assumptions conclusion)
         (indirect-proof assumptions conclusion)))))
