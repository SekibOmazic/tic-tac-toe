(ns tic-tac-toe.core
  (:gen-class))


(def init-board
  [1 2 3 4 5 6 7 8 9])

(def init-player
  :X)


(defn switch-player
 [player]
 (if (= :X player)
   :O
   :X))


(defn convert-cell
  [cell]
  (if (keyword? cell)
    (name cell)
    cell))


(defn show-board
  [current-board]
  (let [board (map convert-cell current-board)]
    (println (nth board 0) "|" (nth board 1) "|" (nth board 2))
    (println "---------")
    (println (nth board 3) "|" (nth board 4) "|" (nth board 5))
    (println "---------")
    (println (nth board 6) "|" (nth board 7) "|" (nth board 8))))


(defn board-complete?
  [board]
  (every? #{:X :O} board))


(defn make-choice
  "read input and validate based on available cells. Read the docs for some"
  [board]
  (let [input
        (try
          (. Integer parseInt (read-line))
          (catch Exception e nil))]
    (some #{input} board)))


(defn play
  [board player]
  (println "\n")
  (println (str (name player) ": ") " Your choice (a number of the cell)")
  (loop [choice (make-choice board)]
    (if choice
      ;; update board and return it
      (assoc board (dec choice) player)
      ;; invalid choice, try again
      (do
        (println (str (name player) ": ") "choice not available, Please make another one")
        (recur (make-choice board))))))



(defn winning-lists
  "split the board into the list of lists (rows, cols and diagonals)"
  [board]
  (list
   ;; rows
   (take 3 board)
   (take 3 (drop 3 board))
   (take 3 (drop 6 board))
   ;;cols
   (take-nth 3 board)
   (take-nth 3 (drop 1 board))
   (take-nth 3 (drop 2 board))
   ;; left-to-right diagonal
   (take-nth 4 board)
   ;; right-to-left diagonal
   (take-nth 2 (drop-last 2 (drop 2 board)))
   ))


(defn has-winner?
  [triplet]
  (if (every? #{:X} triplet)
    :X
    (if (every? #{:O} triplet)
      :O)))


(defn get-winner
  [board]
  (->> board             ;; take the board
       winning-lists     ;; split it into the winning lists
       (map has-winner?) ;; map items to the winning player or nil
       (filter #{:X :O}) ;; filter out nil values
       first))           ;; take the first value if any or nil


(defn game
  [initial-board initial-player]
  (loop [board initial-board
         player initial-player]
    (let [winner (get-winner board)]
      (show-board board)
      (cond
        winner (println "Player " (name winner) " won!")
        (board-complete? board) (println "Draw!")
        :else
        (recur
         (play board player)
         (switch-player player))))))


(defn -main
  "The magic tic tac toe game. Console version only"
  [& args]
  (game init-board init-player))
