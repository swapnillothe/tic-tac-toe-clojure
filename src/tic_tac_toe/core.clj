(ns tic-tac-toe.core)
(require '[clojure.set :refer :all]
         '[clojure.string :as str])

(def winning-combs #{#{1 2 3} #{4 5 6} #{7 8 9} #{1 4 7} #{2 5 8} #{3 6 9} #{1 5 9} #{3 5 7}})

(def symbols ["X" "O"])

(def blank-space " ")

(def pipe-separator " | ")

(defn won? [moves]
  (some #(subset? % moves) winning-combs))

(defn add-move [move player]
  (conj player {:moves (conj (:moves player) move)}))

(defn read-promt [msg]
  (do (println msg) (read)))

(defn ask-move [player]
  (read-promt (str/join (:name player) "'s move")))

(defn ask-player []
  (read-promt "Enter player name"))

(defn get-player-detail [symbol]
  {:name   (ask-player)
   :moves  #{}
   :symbol symbol})

(defn get-player-details []
  (vec (doall (map get-player-detail symbols))))

(defn already-played? [move & players]
  (some #(.contains (:moves %) move) players))

(defn declare-winner [winner]
  (println (:name winner) "has won with moves :" (:moves winner)))

(defn select-symbol [move default-symbol player]
  (if (.contains (:moves player) move)
    (:symbol player)
    default-symbol))

(defn get-symbol [default-symbol move & players]
  (reduce #(select-symbol move % %2) default-symbol players))

(defn draw-grid [player1 player2]
  (->> (range 1 10)
       (map #(get-symbol blank-space % player1 player2))
       (partition 3)
       (mapv vec)
       (mapv #(str/join pipe-separator %))
       (str/join "\n------\n")
       (println)))

(defn continue [[first-player second-player]]
  (draw-grid first-player second-player)
  (let [played-move (ask-move first-player)]
    (if (already-played? played-move first-player second-player)
      (recur [first-player second-player])
      (let [current-player (add-move played-move first-player)]
        (if (won? (:moves current-player))
          (declare-winner current-player)
          (recur [second-player current-player]))))))

(defn start []
  (continue (get-player-details)))

(start)
