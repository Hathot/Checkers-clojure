(ns checkers.ai
  (:require [checkers.model :as model]))

(defn do-move [old-state] 
  (if(checkers.model/check-available-hits-queen) 
    (checkers.model/play! (get (get-first-hit-queen old-state) 0) (get (get-first-hit-queen old-state) 1))
    (if(checkers.model/check-available-hits) 
      (checkers.model/play! (get (get-first-hit old-state) 0) (get (get-first-hit old-state) 1))
        (get-first-move(checkers.model/play! (get (get-first-hit old-state) 0) (get (get-first-hit old-state) 1)))
    
  )))

(defn get-first-move()

(defn get-first-hit
  ([old-state] (get-first-hit 0 0 {} old-state))
  ([row col first old-state]
    (if(and (= row 0)(= col 8))
      false
      (if (= (checkers.model/get-board-cell row col ))(checkers.model/get-symbol(checkers.model/get-player))
        (if (checkers.model/can-hit? row col) 
          (if (not (= first {}))({row col})
          (if(= row 7)
            (recur 0 (inc col) first old-state)
                 (recur (inc row) col first old-state)
                 
                 ))
        (if(= row 7)
          (recur 0 (inc col)first old-state)
          (recur (inc row) col first old-state))
        )))))

(defn get-first-hit-queen
  ([old-state] (get-first-hit-queen 0 0 {} old-state))
  ([row col first old-state]
    (if(and (= row 0)(= col 8))
      false
      (if (= (checkers.model/get-board-cell row col ))(checkers.model/get-queen-symbol(checkers.model/get-player))
        (if (checkers.model/can-hit-queen? row col) 
          (if (not (= first {}))({row col})
          (if(= row 7)
            (recur 0 (inc col) first old-state)
                 (recur (inc row) col first old-state)
                 
                 ))
        (if(= row 7)
          (recur 0 (inc col)first old-state)
          (recur (inc row) col first old-state))
        )))))