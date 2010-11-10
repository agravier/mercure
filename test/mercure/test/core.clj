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

(defn make-comment
  ([s] (str comment-char s))
  ([] (str comment-char  "a comment")))

(def fleet-str (str ennemy-char " 15 0 1 30 2"))
(def prefixed-fleet-str (str fleet-char " " fleet-str))
(def fleet-rec (Fleet. :ennemy 15 0))
(def fleet-vec-1-1 [fleet-rec 1 (+ 2 @current-turn)]) ; returned by read-fleet
(def fleet-vec-1-2 [(+ 2 @current-turn) fleet-rec]) ; recorded by
                  ; update-planet-incoming
(def fleet-vec-2-str (str fleet-char " " ally-char " 11 2 1 30 3"))
(def fleet-vec-2-1 [(Fleet. :ally 11 2) 1 (+ 3 @current-turn)])
(def fleet-vec-2-2 [(+ 3 @current-turn) (Fleet. :ally 11 2)])

(def planet-str "3.14 2.71 0 15 5")
(def prefixed-planet-str (str planet-char " " planet-str))
(def planet-rec (Planet. 3.14 2.71 :neutral 15 5 nil))
(def planet-rec-with-fleets (assoc planet-rec :incoming (sorted-set fleet-vec-1-2 fleet-vec-2-2)))
(def planets-test-rec-2-str (str planet-char " 7 9 2 34 2"))
(def planets-test-rec-2 (Planet. 7 9 :ennemy 34 2 (sorted-set)))
(def planets-test
     [(Planet. 3.14 2.71 :neutral 15 5 (sorted-set))
      (Planet. 0 0 :ally 34 2 (sorted-set))
      planets-test-rec-2])
;; fixture to reinit planets-test
(defn reset-planets [f]
  (dosync (ref-set planets planets-test))
  (f))

;; for read-game-state-t
(def game-update-vec [(str comment-char " something something")
                      ""
                      prefixed-planet-str
                      (str planet-char " 0 0 1 34 2")
                      (str planet-char " 7 9 2 34 2")
                      ""
                      (str planet-char " 28.1 2 1 10 4")
                      (str fleet-char " " ennemy-char " 14 1 1 13 3")
                      prefixed-fleet-str
                      ])

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
  (is (= [planet-rec] (read-planet planet-str))))

(deftest read-status-line-t
  (is (thrown? Exception (read-status-line " invalid line ")))
  (is (= nil (read-status-line (make-comment))))
  (is (= nil (read-status-line "")))
  (atticus.mock/expects
   [(read-fleet [arg]         ; mock read-plane
                (atticus.mock/once ; has to be called once only
                 (is (= arg fleet-str) "argument to read-planet")
                 fleet-vec-1-1))]            ; return value
   (is (= fleet-vec-1-1 (read-status-line prefixed-fleet-str))))
  (atticus.mock/expects
   [(read-planet [arg]         ; mock read-plane
                (atticus.mock/once ; has to be called once only
                 (is (= arg planet-str) "argument to read-planet")
                 planet-rec))]        ; return value
   (is (= planet-rec (read-status-line prefixed-planet-str)))))

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
     (update-game-state planet-rec 3)))
  (testing "case 2: [fleet, dest, etarrival]"
    (atticus.mock/expects
     [(update-planet-incoming [f dest eta]
                              (atticus.mock/once
                               (is (and (= f (first fleet-vec-1-1))
                                        (= dest (second fleet-vec-1-1))
                                        (= eta (nth fleet-vec-1-1 2)))
                                   "arguments to update-planet-incoming")))]
     (apply update-game-state (conj fleet-vec-1-1 42)))))

(deftest read-game-state-t ; TODO: Test more
  (atticus.mock/expects
   [(update-game-state [p id & rest] (atticus.mock/times 6))]
   (doall (read-game-state game-update-vec))))

(deftest travel-time-t
  (let [p00 (Planet. 0 0 :Ally 0 0 nil)
        p00f (Planet. 0.0001 0.0001 :Ally 0 0 nil)
        p10 (Planet. 1 0 :Ally 0 0 nil)
        p01 (Planet. 0 1 :Ally 0 0 nil)
        p22 (Planet. 1 2 :Ally 0 0 nil)
        pfloat (Planet. 112.84 37.7 :Ally 0 0 nil)]
    (is (= (travel-time p00 p00) 0))
    (is (= (travel-time p10 p00) (travel-time p01 p00)
           (travel-time p00 p10) (travel-time p00 p01) 1))
    (is (= (travel-time p00f p00) 1))
    (is (= (travel-time p22 pfloat) 118))))

(deftest planet-to-str-t
  (is (= prefixed-planet-str (planet-to-str planet-rec)) "With a neutral planet")
  (is (= planets-test-rec-2-str (planet-to-str planets-test-rec-2))
      "With an ennemy planet")
  (is (= (str planet-char " 3.14 2.71 1 15 5")
         (planet-to-str (Planet. 3.14 2.71 :ally 15 5 nil)))
      "With an allied planet"))


(deftest incoming-fleets-to-vecstr-t
  (let [incoming-fleets-to-vecstr-t-input [planet-rec-with-fleets
                                           1
                                           (assoc planets-test 1
                                                 planet-rec-with-fleets)]
        incoming-fleets-to-vecstr-t-output [prefixed-fleet-str
                                            fleet-vec-2-str]]
    (atticus.mock/expects
     [(travel-time [p1 p2] (atticus.mock/times 2 30))]
     (is (= (apply incoming-fleets-to-vecstr incoming-fleets-to-vecstr-t-input)
            incoming-fleets-to-vecstr-t-output)))
    (is (= (incoming-fleets-to-vecstr planet-rec 1 planet-rec-with-fleets)
           []))))

(deftest all-fleets-in-transit-to-vecstr-t
  (atticus.mock/expects
   [(incoming-fleets-to-vecstr [p i vp] (atticus.mock/times 4 ['test]))]
   (= (all-fleets-in-transit-to-vecstr
       (conj planets-test planet-rec-with-fleets)) ['test 'test]))
  (= (all-fleets-in-transit-to-vecstr planets-test) [])
  (= (all-fleets-in-transit-to-vecstr []) []))

(deftest all-planets-to-vecstr-t
  (atticus.mock/expects
   [(planet-to-str [p] (atticus.mock/times (count planets-test) 'a))]
   (= (all-planets-to-vecstr planets-test)
      (vec (repeat (count planets-test) 'a)))
   (= (all-planets-to-vecstr []) [])))

(deftest game-state-to-vecstr-t
  (atticus.mock/expects
   [(all-planets-to-vecstr [v] (atticus.mock/once ['a 'b 'c]))
    (all-fleets-in-transit-to-vecstr [v] (atticus.mock/once ['d 'e]))]
   (is (= (game-state-to-vecstr 'whatever) ['a 'b 'c 'd 'e])))
  (is (= (game-state-to-vecstr []) [])))

(run-tests 'mercure.test.core)
