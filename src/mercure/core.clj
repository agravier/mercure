(ns mercure.core
  (:use [clojure.contrib.str-utils :only (re-split)]))

(def comment-char \#)
(def planet-char \P)
(def fleet-char \F)
(def ally-char \1)
(def ennemy-char \2)

(defrecord Fleet [player nships origin])

;; the game can be a vector of planets and a turn nb. :incoming is a
;; ref to an ordered set of [turn of arrival, player, number of ships,
;; origin]

(defrecord Planet [x y player nships growth incoming]) 

(def planets (ref []))
(def current-turn (ref 1))

(defn sym [n]
  "Returns the symbol for the player number"
  (cond (= n 0) :neutral
        (= n 1) :ally
        (= n 2) :ennemy
        :else (throw (new Exception (str "Invalid player number: " n)))))

(defn read-numbers-str [ss]
  "Returns a seq of the numbers in the numbers-only string."
  (let [tss (.trim ss)]
    (if (not (.isEmpty tss))
      (filter number? (map read-string (re-split #"\p{Blank}+" tss)))
      ())))

(defn read-planet [ss]
  (let [pldata (read-numbers-str ss)]
    (if (not= (count pldata) 5)
      (throw (new Exception (str "Invalid planet description: " ss)))
      (Planet.
        (first pldata) (second pldata) ; x y
        (sym (nth pldata 2)) ; player
        (nth pldata 3) (nth pldata 4) nil)))) ; nships growth incoming

(defn read-fleet [ss]
  "Returns the [fleet(player, nb of ships, origin), its dest, turn of arrival]"
  (let [fldata (read-numbers-str ss)]
    (if (not= (count fldata) 6)
      (throw (new Exception (str "Invalid planet description: " ss)))
      (list (Fleet. (sym (first fldata)) (second fldata) (nth fldata 2))
            (nth fldata 3)
            (+ (nth fldata 5) @current-turn)))))

;; TODO: replace the #"#" by a re using comment-char. how?
(defn read-status-line [[marker & _ :as s]]
  (if (not (.isEmpty s))
   (let [cdata (.trim (first (re-split #"#" (.substring s 1) 1)))]
     (cond
      (= marker planet-char) (read-planet cdata)
      (= marker fleet-char) (read-fleet cdata)
      (or (.isEmpty cdata) (= marker comment-char)) nil
      :else (throw (new Exception (str "Invalid map line: "s)))))))

(defn update-game-state
  "Updates the planets data. params: [planet, id] or [fleet, dest, etarrival]"
  ([{:keys [x y player nships growth incoming] :as p} id]
     (dosync
      (alter planets
             assoc id (assoc p :incoming (:incoming (get @planets id))))))
  ([{:keys [player nships] :as fleet} destid eta]
     (dosync
      (let [destp (get @planets destid)]
        (alter planets assoc destid
               (assoc destp :incoming (conj (:incoming destp) fleet)))))))

(defn -main [& args]
  )

