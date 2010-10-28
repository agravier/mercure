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
(def fleet-vec-1 [fleet-rec 1 (+ 2 @current-turn)]) ; returned by read-fleet
(def fleet-vec-2 [(+ 2 @current-turn) fleet-rec]) ; recorded by update-game-state

(def planet-str "3.14 2.71 0 15 5")
(defn stub-planet
  [] (str planet-char " " planet-str))
(def planet-rec (Planet. 3.14 2.71 :neutral 15 5 nil))
(def planet-rec-with-fleets (assoc planet-rec :incoming [fleet-vec-2]))

(def planets-test
     [(Planet. 3.14 2.71 :neutral 15 5 [])
      (Planet. 0 0 :ally 34 2 [])
      (Planet. 7 9 :ennemy 34 2 [])])
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
  (is (= fleet-vec-1 (read-fleet fleet-str))))

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
                 fleet-vec-1))]            ; return value
   (is (= fleet-vec-1 (read-status-line (stub-fleet)))))
  (atticus.mock/expects
   [(read-planet [arg]         ; mock read-plane
                (atticus.mock/once ; has to be called once only
                 (is (= arg planet-str) "argument to read-planet")
                 planet-rec))]        ; return value
   (is (= planet-rec (read-status-line (stub-planet))))))

(deftest update-game-state-t
  (testing "case 1: [planet, id]"
    (update-game-state planet-rec 3)
    (is (= (nth @planets 3) planet-rec)))
  (testing "case 2: [fleet, dest, etarrival]"
    (apply update-game-state fleet-vec-1)
    (is (= (:incoming (nth @planets 1)) [fleet-vec-2]))))

(run-tests 'mercure.test.core)
