(ns checkers.model
  (:require [noir.session :as session]))

(def empty-board [[\o \  \o \  \o \  \o \ ]
                   [\  \o  \  \o  \  \o  \  \o ] 
                   [\o \  \o \  \o \  \o \ ]
                   [\  \- \  \- \  \- \  \-]
                   [\- \  \- \  \- \  \- \ ]
                   [\  \x \  \x \  \x \  \x]
                   [\x  \  \x  \  \x  \  \x  \ ]
                   [\  \x \  \x \  \x \  \x]])



(def init-state {:board empty-board :player \x :selected-row -1 :selected-col -1 :locked false :message ""})

(defn select-row[] 
  (:selected-row (session/get :game-state)))
(defn select-col[] 
  (:selected-col (session/get :game-state)))

(defn abs [x] 
       (if (< x 0) (- x) x))

(defn reset-game! []
  (session/put! :game-state init-state))

(defn get-board []
  (:board (session/get :game-state)))

(defn get-board-cell 
  ([row col]
    (get-board-cell (get-board) row col))
  ([board row col]
    (get-in board [row col])))

(defn get-player []
  (:player (session/get :game-state)))


(defn get-message [](
    :message (session/get :game-state)))

(defn other-player 
  ([] (other-player (get-player)))
  ([player] (if (= player \x) \o \x))) 

(defn get-symbol [player row col](
                                   if(= player \o) (if(= row 7) \O \o) (if(= player \x) (if(= row 0) \X \x) 
                                                                         player)))

(defn get-queen-symbol[player] (
                                 if(= player \x) \X \O
                                 ))

(defn change-value[board row col value](
                                         assoc-in board [row col] value 
                                         ))

(defn get-value-between[value1 value2] (
                                         / (+ value1 value2) 2
                                         ))

;;checks if cell is a queen cell
(defn is-queen?[cell] (
                        or(= cell \X)(= cell \O)
                        ))


                           
(defn get-old-state-with-message[old-state message] 
  "adds a message to the current state"
  {:board (:board old-state)
   :player (:player old-state) 
   :selected-row (:selected-row old-state) 
   :selected-col (:selected-col old-state)
   :locked (:locked old-state)
   :message message
   }      
  )


(defn winner?
  "checks if there is a winner, if so, returns his symbol"
  ([] (winner? (get-board))) 
  ([board]
    (if (winner? board \x)
      \o 
      (if (winner? board \o)
        \x)
      ))
  ([board player]
    (if(= (.indexOf (str(clojure.string/lower-case(clojure.string/join board))) (str player)) -1) true)))


(defn get-value-based-on-player
  "gets the row-moving value based on player"
  ([] (get-value-based-on-player (get-player)))
  ([player] (if (= player \o) 1 -1))) 

        

(defn check-available-hits 
  " checks entire board for current-player symbols and calls ' can-hit?' foreach found"
  ([] (check-available-hits 0 0))
  ([row col] 
    (if(and (= row 0)(= col 8))
      false
      (if(and (can-hit? row col) (or (= (get-board-cell row col)(get-player))
                                     (= (get-board-cell row col) (get-queen-symbol(get-player)))))
        true
        (if(= row 7)
          (recur 0 (inc col))
          (recur (inc row) col))))  
    ))
 
(defn can-hit? [row col] 
                           " checks if a cell can hit the other player"
                           (or(is-hit-valid? row col (+ row 2) ( + col 2))
                           (or(is-hit-valid? row col (- row 2) (+ col 2))
                              (or (is-hit-valid? row col (+ row 2)(- col 2)) 
                                  (is-hit-valid? row col (- row 2)(- col 2))))
                           
                           ))

(defn is-hit-valid? [source-row source-col destin-row destin-col] 
  "checks if the hit that is given is actually valid"
  (and 
    (or 
      (or 
        (or 
          (and 
            (and (= 2 (- destin-col source-col))
                 (= 2 (- destin-row source-row))) 
            (or (= (other-player) (get-board-cell (- destin-row 1)(- destin-col 1)))
                (= (get-queen-symbol (other-player)) (get-board-cell (- destin-row 1)(- destin-col 1)))))
          (and 
            (and (= -2 (- destin-col source-col))
                 (= 2 (- destin-row source-row))) 
            (or (= (other-player) (get-board-cell (- destin-row 1)(+ destin-col 1)))
                (= (get-queen-symbol (other-player)) (get-board-cell (- destin-row 1)(+ destin-col 1))))))
        (and 
          (and (= -2 (- destin-col source-col))
               (= -2 (- destin-row source-row))) 
          (or (= (other-player) (get-board-cell (+ destin-row 1)(+ destin-col 1)))
              (= (get-queen-symbol (other-player)) (get-board-cell (+ destin-row 1)(+ destin-col 1))))))
      (and(and (= 2 (- destin-col source-col))
               (= -2 (- destin-row source-row))) 
          (or (= (other-player) (get-board-cell (+ destin-row 1)(- destin-col 1)))
              (= (get-queen-symbol (other-player)) (get-board-cell (+ destin-row 1)(- destin-col 1))))))
    (= (get-board-cell destin-row destin-col) \-)))



(defn is-move-valid? [source-row source-col destin-row destin-col]   
  "checks if just a move done by a player is valid"
  (and (= (get-value-based-on-player) (- destin-row source-row))
       (or (= (get-value-based-on-player) (- destin-col source-col))
           (= (get-value-based-on-player) (- source-col destin-col)))
       ))


(defn check-available-hits-queen
  "checks entire board for queens for current player and calls 'can-hit-queen?' foreach queen found"
  ([] (check-available-hits-queen 0 0))
  ([row col]
    (if(and (= row 0)(= col 8))
      false
      (if (= (get-board-cell row col )(get-queen-symbol(get-player)))
             (if (can-hit-queen? row col) true 
               (if(= row 7)
               (recur 0 (inc col))
               (recur (inc row) col)))
             (if(= row 7)
               (recur 0 (inc col))
               (recur (inc row) col)))
      )))


(defn can-hit-queen? 
"checks if a given cell with a queen on it can do a hit-move"
  ([row col] (can-hit-queen? row col 0 false))
  ([row col i value] 
    (if (= i 8)
      value
      (if (is-queen-hit-valid? row col (+ row i)(+ col i))
        (recur row col 8 true)
        (if (is-queen-hit-valid? row col (- row i)(+ col i))
          (recur row col 8 true)
          (if (is-queen-hit-valid? row col (+ row i)(- col i))
            (recur row col 8 true)
            (if(is-queen-hit-valid? row col (- row i)(- col i))
              (recur row col 8 true)
              (recur row col (inc i) false)
              )
            )
          )
        )
      )))


(defn is-queen-hit-valid? 
  ([s-row s-col d-row d-col] ( if(= (abs(- s-row d-row)) (abs(- s-col d-col)))
                               (is-queen-hit-valid? 
                               (+ s-row (if(< s-row d-row) 1 -1 )) 
                               (+ s-col (if(< s-col d-col) 1 -1 ))
                               d-row 
                               d-col false)
                               false))
  ([s-row s-col d-row d-col hit?]
    (if (and (= s-row d-row) (= s-col d-col))
      hit?
      (if (= (get-board-cell s-row s-col) \-)
        (recur (+ s-row (if(< s-row d-row) 1 -1 )) 
               (+ s-col (if(< s-col d-col) 1 -1 ))
               d-row
               d-col
               hit?)
        (if (or (= (get-board-cell s-row s-col) (other-player)) (= (get-board-cell s-row s-col) (get-queen-symbol(other-player))))
          (if (= (get-board-cell (+ s-row (if(< s-row d-row) 1 -1 )) 
                                 (+ s-col (if(< s-col d-col) 1 -1 ))) \-)
            (recur (+ s-row (if(< s-row d-row) 1 -1 )) 
                   (+ s-col (if(< s-col d-col) 1 -1 ))
                   d-row
                   d-col
                   true)
            (recur 0 0 0 0 false))
          (if (= (get-board-cell s-row s-col) \- ) 
            (recur (+ s-row (if(< s-row d-row) 1 -1 )) 
                   (+ s-col (if(< s-col d-col) 1 -1 ))
                   d-row
                   d-col
                   true)
            (recur 0 0 0 0 false)))))))

(defn is-queen-move-valid? 
  ([s-row s-col d-row d-col] ( if(= (abs(- s-row d-row)) (abs(- s-col d-col)))
                               (is-queen-hit-valid? 
                               (+ s-row (if(< s-row d-row) 1 -1 )) 
                               (+ s-col (if(< s-col d-col) 1 -1 ))
                               d-row 
                               d-col true)
                               false))
  ([s-row s-col d-row d-col value]
    (if (and (= s-row d-row) (= s-col d-col))
      value
      (if (= (get-board-cell s-row s-col) \-)
        (recur (+ s-row (if(< s-row d-row) 1 -1 )) 
               (+ s-col (if(< s-col d-col) 1 -1 ))
               d-row
               d-col
               true)
        (if (or (or (= (get-board-cell s-row s-col) (other-player)) (= (get-board-cell s-row s-col) (get-queen-symbol(other-player))))
                (or (= (get-board-cell s-row s-col) (get-player)) (= (get-board-cell s-row s-col) (get-queen-symbol(get-player)))))
          (recur 0 0 0 0 false)
          )))))
               


(defn remove-all-pieces-on-line [board source-row source-col destin-row destin-col](
                                                                                     if(and (= source-row destin-row)(= source-col destin-col))                                                                      
                                                                                     (change-value board destin-row destin-col \-)
                                                                                     (recur (change-value board source-row source-col \- ) (+ source-row (if(< source-row destin-row) 1 -1 )) (+ source-col (if(< source-col destin-col) 1 -1 )) destin-row destin-col)
                                                                                     ))


(defn new-state [row col old-state] (
                                      if(winner? (:board old-state))
                                      old-state
                                      (if (and (= (get-board-cell (:board old-state) row col) \-)(> (:selected-row old-state) -1))
                                        (if (or (check-available-hits) (check-available-hits-queen))
                                          (if (is-queen? (get-board-cell (:board old-state) (:selected-row old-state) (:selected-col old-state))) 
                                            (if (is-queen-hit-valid? (:selected-row old-state)(:selected-col old-state) row col) 
                                              {:board (change-value 
                                                      (change-value (remove-all-pieces-on-line (:board old-state) (:selected-row old-state)(:selected-col old-state) row col) (:selected-row old-state) (:selected-col old-state) \-)
                                                      row col (get-queen-symbol (:player old-state)))
                                             ;{:board (get-board) 
                                               :player (other-player(:player old-state))
                                               :selected-row -1 
                                               :selected-col -1  
                                               :locked false
                                             } 
                                              (get-old-state-with-message old-state "invalid queen hit")
                                             )
                                            (if (is-hit-valid? (:selected-row old-state) (:selected-col old-state) row col) 
                                              {:board 
                                               (change-value 
                                                 (change-value 
                                                   (change-value (:board old-state) (get-value-between (:selected-row old-state) row) (get-value-between (:selected-col old-state) col) \-)
                                                   (:selected-row old-state) (:selected-col old-state)  \-)
                                                 row col
                                                 (get-symbol (:player old-state) row col))
                                               
                                               :player (if(can-hit? row col)(:player old-state)(other-player (:player old-state)))
                                               :selected-row (if (can-hit? row col) row -1) 
                                               :selected-col (if (can-hit? row col) col -1)  
                                               :locked (if (can-hit? row col) true false)
                                               }
                                              (get-old-state-with-message old-state "invalid hit")
                                              ))
                                          
                                          ;;moves:
                                          (if (is-queen? (get-board-cell (:board old-state) (:selected-row old-state) (:selected-col old-state)))
                                            (if (is-queen-move-valid? (:selected-row old-state)(:selected-col old-state) row col) 
                                              {:board (change-value 
                                                      (change-value (:board old-state) (:selected-row old-state) (:selected-col old-state) \-)
                                                      row col (get-queen-symbol (:player old-state)))
                                             ;{:board (get-board) 
                                             :player (other-player (:player old-state))
                                             :selected-row -1
                                             :selected-col -1
                                             :locked false
                                             }
                                              (get-old-state-with-message old-state "invalid queen move")
                                              )
                                            (if (is-move-valid? (:selected-row old-state) (:selected-col old-state) row col) 
                                              {:board (change-value 
                                                      (change-value (:board old-state) (:selected-row old-state) (:selected-col old-state) \-)
                                                      row col (get-symbol (:player old-state) row col))
                                             :player (other-player (:player old-state))
                                             :selected-row -1
                                             :selected-col -1
                                             :locked false
                                             }
                                              (get-old-state-with-message old-state "invalid move"))))
                                        
                                         (if(and (or (= (get-board-cell (:board old-state) row col) (:player old-state)) 
                                                    (= (get-board-cell (:board old-state) row col) (get-queen-symbol (:player old-state))))
                                                
                                                (not (:locked old-state)))
                                          {:board (:board old-state)
                                           :player (:player old-state) 
                                           :selected-row row 
                                           :selected-col col
                                           :locked false
                                           }
                                          old-state))))
                                      


(defn new-state2 [row col old-state] (
                                      if(winner? (:board old-state))
                                      old-state
                                      (if (and (= (get-board-cell (:board old-state) row col) \-)(> (:selected-row old-state) -1))
                                        
                                        (if (not (is-queen? (get-board-cell (:board old-state) (:selected-row old-state) (:selected-col old-state))))
                                          (if (and (is-move-valid? (:selected-row old-state) (:selected-col old-state) row col) 
                                                   (not (check-available-hits)))
                                            {:board (change-value 
                                                      (change-value (:board old-state) (:selected-row old-state) (:selected-col old-state) \-)
                                                      row col (get-symbol (:player old-state) row col))
                                             :player (other-player (:player old-state))
                                             :selected-row -1
                                             :selected-col -1
                                             }
                                            (if (is-hit-valid? (:selected-row old-state) (:selected-col old-state) row col)
                                              {:board 
                                               (change-value 
                                                 (change-value 
                                                   (change-value (:board old-state) (get-value-between (:selected-row old-state) row) (get-value-between (:selected-col old-state) col) \-)
                                                   (:selected-row old-state) (:selected-col old-state)  \-)
                                                 row col
                                                 (get-symbol (:player old-state) row col))
                                               
                                               :player (if(can-hit? row col)(:player old-state)(other-player (:player old-state)))
                                               :selected-row (if (can-hit? row col) row -1) 
                                               :selected-col (if (can-hit? row col) col -1)  
                                               :locked (if (can-hit? row col) true false)
                                               }
                                              old-state
                                              ))       
                                          (if (and (is-queen-move-valid? (:selected-row old-state)(:selected-col old-state) row col) (not (check-available-hits-queen)))
                                            {:board (change-value 
                                                      (change-value (:board old-state) (:selected-row old-state) (:selected-col old-state) \-)
                                                      row col (get-queen-symbol (:player old-state)))
                                             ;{:board (get-board) 
                                             :player (other-player (:player old-state))
                                             :selected-row -1
                                             :selected-col -1
                                             } 
                                            (if (and (is-queen-hit-valid? (:selected-row old-state)(:selected-col old-state) row col) (check-available-hits-queen))
                                            {:board (change-value 
                                                      (change-value (remove-all-pieces-on-line (:board old-state) (:selected-row old-state)(:selected-col old-state) row col) (:selected-row old-state) (:selected-col old-state) \-)
                                                      row col (get-queen-symbol (:player old-state)))
                                             ;{:board (get-board) 
                                             :player (other-player (:player old-state))
                                             :selected-row -1
                                             :selected-col -1
                                             } old-state
                                            )
                                            ))
                                        
                                        (if(and (or (= (get-board-cell (:board old-state) row col) (:player old-state)) 
                                                    (= (get-board-cell (:board old-state) row col) (get-queen-symbol (:player old-state))))
                                                
                                                (not (:locked old-state)))
                                          {:board (:board old-state)
                                           :player (:player old-state) 
                                           :selected-row row 
                                           :selected-col col
                                           }
                                          old-state)
                                        )))



(defn play! [row col]
  (session/swap! (fn [session-map]
                   (assoc session-map :game-state 
                          (new-state row col (:game-state session-map))))))