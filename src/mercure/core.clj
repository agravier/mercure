(ns mercure.core
  (:use [clojure.contrib.math :only (sqrt ceil)]
        [clojure.contrib.str-utils :only (re-split)]))

(def planets (ref []))
(def current-turn (ref 1))

(def comment-char \#)
(def planet-char \P)
(def fleet-char \F)
(def ally-char \1)
(def ennemy-char \2)
(def end-of-io-char \g)
(def end-of-io-str "go")

(def initial-turn-duration-millis 3000)
(def turn-duration-millis 2000)

;;;; Data structures

(def end-of-io :end)

(defn sym
  "Returns the symbol for the player number"
  [n]
  (cond (= n 0) :neutral
        (= n 1) :ally
        (= n 2) :ennemy
        (= n :neutral) 0
        (= n :ally) 1
        (= n :ennemy) 2
        :else (throw (new Exception (str "Invalid player number: " n)))))

(defrecord Fleet [player nships origin]
  java.lang.Comparable   ; needed for the ordering of the sets of [eta, fleet]
  (compareTo [self o] 0)); could be (- nships (:nships o)))

;; the game can be a vector of planets and a turn nb. :incoming is
;; a sorted set of [etarrival, {player, number of ships, origin}]
(defrecord Planet [x y player nships growth incoming]
  java.lang.Object
  (toString [self]
            (reduce str
                    (interpose " "
                               [planet-char x y (sym player) nships growth]))))

;;;; Input transformations

(defn read-numbers-str
  "Returns a seq of the numbers in the numbers-only string."
  [ss]
  (let [tss (.trim ss)]
    (if (not (.isEmpty tss))
      (filter number? (map read-string (re-split #"\p{Blank}+" tss)))
      ())))

(defn read-planet [ss]
  (let [pldata (read-numbers-str ss)]
    (if (not= (count pldata) 5)
      (throw (new Exception (str "Invalid planet description: " ss)))
      [(Planet.
        (first pldata) (second pldata)       ; x y
        (sym (nth pldata 2))                 ; player
        (nth pldata 3) (nth pldata 4) nil)]))) ; nships growth incoming

(defn read-fleet
  "Returns the [fleet(player, nb of ships, origin), its dest, turn of arrival]"
  [ss]
  (let [fldata (read-numbers-str ss)]
    (if (not= (count fldata) 6)
      (throw (new Exception (str "Invalid planet description: " ss)))
      [(Fleet. (sym (first fldata)) (second fldata) (nth fldata 2))
       (nth fldata 3)
       (+ (nth fldata 5) @current-turn)])))

;; TODO: replace the #"#" by a re using comment-char. how?
(defn read-status-line
  "Reads a string and returns a planet, a fleet and its dest and eta, or nil"
  [[marker & _ :as s]]
  (if (not (.isEmpty s))
   (let [cdata (.trim (first (re-split #"#" (.substring s 1) 1)))]
     (cond
      (= marker planet-char) (read-planet cdata)
      (= marker fleet-char) (read-fleet cdata)
      (or (.isEmpty cdata) (= marker comment-char)) nil
      :else (throw (new Exception (str "Invalid map line: " s)))))))

(defn add-new-planet
  "Appends the given planet at the end of the planets vector"
  [p]
  (dosync (alter planets conj p)))

;; TODO: when releasing, change that to nop
(defn update-planet-data
  "Updates an existing planet in the planets vector with the given data"
  [{:keys [x y player nships growth incoming] :as p} id]
  (if (>= id (count @planets))
    (throw (Exception. (str "Tried to update planet id " id ", which
    does not exist")))
    (dosync
     (alter planets
            assoc id (assoc p :incoming (:incoming (get @planets id)))))))

(defn update-planet-incoming
  "Adds the given [eta, fleet] to the :incoming collection of the
  planet selected by the given id, if not already present"
  [{:keys [player nships] :as fleet} destid eta]
  ;; note: no duplicate guaranteed by the use of a sorted set
  (dosync
   (let [destp (get @planets destid)]
     (alter planets assoc destid
            (assoc destp :incoming (conj (:incoming destp) [eta fleet]))))))

(defn update-game-state
  "Updates the planets data. params: [planet, id] or [fleet, dest, etarrival, _]. The underscore stands for the line number, given by read-game-state, but only useful for planets."
  ([pdata id]
     (if (< id (count @planets))
       (update-planet-data pdata id)
       (add-new-planet pdata)))
  ([fleet destid arrival _]
     (update-planet-incoming fleet destid arrival)))

(defn read-game-state
  "Reads a vector of game state update lines to update the game state. The planets need to all come before the fleet, as per the game spec. There can be empty and comments lines anywhere."
  [vecstr]
  (let [vectok (filter #(not (nil? %)) (map read-status-line vecstr))]
    (map #(apply update-game-state %)
         (map #(conj %1 %2) vectok (iterate inc 1)))))

(defn acquire-state-lines  ; TODO: test
  "Reads *in* line-by-line until end-of-io-str is encountered, and passes the vecto of read strings the read-game-state function."
  []
  (read-game-state (loop [l (read-line)
                          v []]
                     (if (= end-of-io-str l)
                       v
                       (recur (read-line) (conj v l))))))

;;;; Common computations

(defn travel-time [p1 p2]
  (let [dx (- (:x p1) (:x p2))
        dy (- (:y p1) (:y p2))]
    (int (ceil (sqrt (+ (* dx dx) (* dy dy)))))))

;;;; Output transformations

(defn planet-to-str [p]
  (.toString p))

(defn incoming-fleets-to-vecstr
  "returns a vector of string representations of all fleets in transit to the planet given in parameter. Each string represents one fleet and conforms to the gme state representation specification. The game state vector is given in parameter"
  [planet idplanet vecplanets]
  (vec (map #(let [fl (nth % 1)
                   eta (nth % 0)]
               (reduce str
                       (interpose " " [fleet-char (sym (:player fl))
                                       (:nships fl) (:origin fl) idplanet
                                       (travel-time (nth vecplanets
                                                         (:origin fl))
                                                    planet)
                                       (- eta @current-turn)])))
            (:incoming planet))))

(defn all-fleets-in-transit-to-vecstr
  "Returns a vector of string representations of all fleets in transit at the current turn."
  [vecplanets]
  (let [vvecstr (map incoming-fleets-to-vecstr
                     vecplanets (iterate inc 0) (repeat vecplanets))]
    (if (< (count vvecstr) 1) [] (reduce into vvecstr))))

(defn all-planets-to-vecstr
  [vecplanets] 
  (map planet-to-str vecplanets))

(defn game-state-to-vecstr
  "Returns a vector of the all the strings representing the current state of the game."
  [vecplanets] 
  (into (all-planets-to-vecstr vecplanets)
        (all-fleets-in-transit-to-vecstr vecplanets)))

(defn order [fleet destid]
  (str (:origin fleet) " " destid " " (:nships fleet)))

(defn issue-orders [v] ; TODO: test
  (map println (conj v end-of-io-str)))

;;;; Time management

(defn make-countdown-f ; TODO: test
  "Returns a function that gives the number of milliseconds before the time in the parameter (in milliseconds) has elapsed"
  [t]
  (let [time-limit (+ t (System/currentTimeMillis))]
    (fn [] (- time-limit (System/currentTimeMillis)))))

;;;; Game loop

(defn run ; TODO: test
  "Runs the game turns. The agent-f parameter is a function that takes a countdown function as parameter. The coutdown function returns the number of milliseconds remaining before the end of the turn."
  [agent-f]
  (loop [s (acquire-state-lines)
         countdown-f (make-countdown-f initial-turn-duration-millis)]
    (do (read-game-state s)
        (dosync (alter current-turn inc))
        (issue-orders (agent-f countdown-f))
        (recur (acquire-state-lines) (make-countdown-f turn-duration-millis)))))
