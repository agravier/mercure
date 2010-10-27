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

(def planet-str "3.14 2.71 0 15 5")
(defn stub-planet
  [] (str planet-char " " planet-str))
(def planet-rec (Planet. 3.14 2.71 :neutral 15 5 nil))

(def fleet-str-no-player "15 0 1 12 2")
(def fleet-str (str ennemy-char " " fleet-str-no-player))
(defn stub-fleet
  "player keyword in param"
  ([k] (str fleet-char " "
            (if (= k :ally) ally-char ennemy-char)
            " " fleet-str-no-player))
  ([] (str fleet-char " " fleet-str)))
(def fleet-rec (Fleet. :ennemy 15 0))
(def fleet-vec [fleet-rec 1 (+ 2 @current-turn)])

(def planets-test
     [(Planet. 3.14 2.71 :neutral 15 5 [])
      (Planet. 0 0 :ally 34 2 [])
      (Planet. 7 9 :ennemy 34 2 [])])
(defn reset-planets []
  (dosync (ref-set planets planets-test)))

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
  (is (= fleet-vec (read-fleet fleet-str))))

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
                 fleet-vec))]            ; return value
   (is (= fleet-vec (read-status-line (stub-fleet)))))
  (atticus.mock/expects
   [(read-planet [arg]         ; mock read-plane
                (atticus.mock/once ; has to be called once only
                 (is (= arg planet-str) "argument to read-planet")
                 planet-rec))]        ; return value
   (is (= planet-rec (read-status-line (stub-planet))))))
