(ns mercure.test.core
  (:use [mercure.core] :reload)
  (:import [mercure.core Fleet Planet])
  (:use [clojure.test])
  (:require [atticus.mock]))

(import 'mercure.core.Planet)
(import 'mercure.core.Fleet)
;;
;; helpers
;;

(defn stub-comment
  ([s] (str comment-char s))
  ([] (str comment-char  "a comment")))

(def fleet-str-no-player "15 0 1 12 2")
(def fleet-str (str ennemy-char " " fleet-str-no-player))
(defn stub-fleet
  "player keyword in param"
  ([k] (str fleet-char " "
            (if (= k :ally) ally-char ennemy-char)
            " " fleet-str-no-player))
  ([] (str fleet-char " " fleet-str)))
(def fleet-rec (Fleet. :ennemy 15 0))
(def fleet-vec-1-1 [fleet-rec 1 (+ 2 @current-turn)]) ; returned by read-fleet
(def fleet-vec-1-2 [(+ 2 @current-turn) fleet-rec]) ; recorded by update-planet-incoming
(def fleet-vec-2-1 [(Fleet. :ally 11 0) 1 (+ 3 @current-turn)])
(def fleet-vec-2-2 [(+ 3 @current-turn) (Fleet. :ally 11 0)])

(def planet-str "3.14 2.71 0 15 5")
(defn stub-planet
  [] (str planet-char " " planet-str))
(def planet-rec (Planet. 3.14 2.71 :neutral 15 5 nil))
(def planet-rec-with-fleets (assoc planet-rec :incoming (sorted-set fleet-vec-1-2)))

(def planets-test-rec-2 (Planet. 7 9 :ennemy 34 2 (sorted-set)))
(def planets-test
     [(Planet. 3.14 2.71 :neutral 15 5 (sorted-set))
      (Planet. 0 0 :ally 34 2 (sorted-set))
      planets-test-rec-2])
;; fixture to reinit planets-test
(defn reset-planets [f]
  (dosync (ref-set planets planets-test))
  (f))

(use-fixtures :each reset-planets)

;;
;; tests
;;
(deftest sym-t
  (is (thrown? Exception (sym nil)))
  (is (thrown? Exception (sym 1.1)))
  (is (thrown? Exception (sym 7)))
  (is (= :ally (sym 1)))
  (is (= :ennemy (sym 2)))
  (is (= :neutral (sym 0))))

(deftest read-numbers-str-t
  (is (= () (read-numbers-str "n0t numb3rs")))
  (is (= (list 1 2.2 0 -99 747) (read-numbers-str " 1 2.2 0 -99 747")))
  (is (= () (read-numbers-str ""))))

(deftest read-fleet-t
  (is (thrown? Exception (read-fleet (str fleet-str " \n" fleet-str))))
  (is (= fleet-vec-1-1 (read-fleet fleet-str))))

(deftest read-planet-t
  (is (thrown? Exception (read-planet (str planet-str " \n" planet-str))))
  (is (= planet-rec (read-planet planet-str))))

(deftest read-status-line-t
  (is (thrown? Exception (read-status-line " invalid line ")))
  (is (= nil (read-status-line (stub-comment))))
  (is (= nil (read-status-line "")))
  (atticus.mock/expects
   [(read-fleet [arg]         ; mock read-plane
                (atticus.mock/once ; has to be called once only
                 (is (= arg fleet-str) "argument to read-planet")
                 fleet-vec-1-1))]            ; return value
   (is (= fleet-vec-1-1 (read-status-line (stub-fleet)))))
  (atticus.mock/expects
   [(read-planet [arg]         ; mock read-plane
                (atticus.mock/once ; has to be called once only
                 (is (= arg planet-str) "argument to read-planet")
                 planet-rec))]        ; return value
   (is (= planet-rec (read-status-line (stub-planet))))))

(deftest add-new-planet-t
  (add-new-planet planet-rec)
  (is (= (nth @planets 3) planet-rec)) "Add planet at the end")

(deftest update-planet-data-t
  (update-planet-data planets-test-rec-2 2)
  (is (= (nth @planets 2) planets-test-rec-2) "Update exiting planet to same")
  (is (thrown? Exception (update-planet-data planet-rec 3))) "Add planet at the end")

(deftest update-planet-incoming-t
  (apply update-planet-incoming fleet-vec-1-1)
  (is (= (:incoming (nth @planets 1)) (sorted-set fleet-vec-1-2)) "Add an incoming fleet to an empty fleet set")
  (apply update-planet-incoming fleet-vec-2-1)
  (is (= (:incoming (nth @planets 1)) (sorted-set fleet-vec-2-2 fleet-vec-1-2)) "Add an incoming fleet to a non-empty fleet set")
  (apply update-planet-incoming fleet-vec-1-1)
  (is (= (:incoming (nth @planets 1)) (sorted-set fleet-vec-2-2 fleet-vec-1-2)) "Duplicate addition test"))

(deftest update-game-state-t ; TODO: Test more
  (testing "case 1: [planet, id]"
    (atticus.mock/expects
     [(update-planet-data [p id] ; mock
                          (atticus.mock/once 
                           (is (and (= p planets-test-rec-2) (= id 2))
                               "arguments to update-planet-data")))]
     (update-game-state planets-test-rec-2 2))
    (atticus.mock/expects
     [(add-new-planet [p]
                      (atticus.mock/once
                       (is (= p planet-rec)
                           "arguments to add-new-planet")))]
     (update-game-state planet-rec 3))
    (testing "case 2: [fleet, dest, etarrival]"
      (atticus.mock/expects
       [(update-planet-incoming [f dest eta]
                                (atticus.mock/once
                                 (is (and (= f (first fleet-vec-1-1))
                                          (= dest (second fleet-vec-1-1))
                                          (= eta (nth fleet-vec-1-1 2)))
                                     "arguments to update-planet-incoming")))]
       (apply update-game-state fleet-vec-1-1)))))

(run-tests 'mercure.test.core)
