(ns mercure.test.core
  (:use [mercure.core] :reload)
  (:use [clojure.test])
  (:require [atticus.mock]))

;;
;; helpers
;;

(defn stub-comment
  ([s] (str comment-char s))
  ([] (str comment-char  "a comment")))

(def planet-str "3.14 2.71 0 15 5")
(defn stub-planet
  [] (str planet-char " " planet-str))
(def planet-struct (struct planet 3.14 2.71 :neutral 15 5 nil))

(def fleet-str-no-player "15 0 1 12 2")
(def fleet-str (str ennemy-char " " fleet-str-no-player))
(defn stub-fleet
  "player keyword in param"
  ([k] (str fleet-char " "
            (if (= k :ally) ally-char ennemy-char)
            " " fleet-str-no-player))
  ([] (str fleet-char " " fleet-str)))
(def fleet-struct (struct fleet :ennemy 15 0))
(def fleet-vec [fleet-struct 1 (+ 2 @current-turn)])

(def planets-test
     [{struct-map planet :x 3.14 :y 2.71
       :player 0 :nships 15 :growth 5 :incoming []}
      {struct planet 0 0 1 34 2 []}
      {struct planet 7 9 2 34 2 []}])
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
  (is (= planet-struct (read-planet planet-str))))

(deftest read-line-t
  (is (thrown? Exception (read-line " invalid line ")))
  (is (= nil (read-line (stub-comment))))
  (is (= nil (read-line "")))
  (atticus.mock/expects
   [(read-fleet [arg]         ; mock read-plane
                (atticus.mock/once ; has to be called once only
                 (is (= arg fleet-str) "argument to read-planet")
                 fleet-vec))]            ; return value
   (is (= fleet-vec (read-line (stub-fleet)))))
  (atticus.mock/expects
   [(read-planet [arg]         ; mock read-plane
                (atticus.mock/once ; has to be called once only
                 (is (= arg planet-str) "argument to read-planet")
                 planet-struct))]        ; return value
   (is (= planet-struct (read-line (stub-planet))))))
