(ns CheckersV1.model
  (:use checkers.model)
  (:use clojure.test))


(deftest abs-test
  (is (> (abs -1) 0))
  (is (> (abs 1) 0))
  (is (> (abs -4) 9)))

(deftest boardRangeTest
  (binding [noir.session/*noir-session* (atom {})]
    (reset-game!)
  (is (get-board-cell 7 7))
  (is (get-board-cell 8 9))))


(deftest remove-all-pieces-on-line-test
  (binding [noir.session/*noir-session* (atom {})]
    (reset-game!)
    (is (remove-all-pieces-on-line (get-board) 8 8 5 5))
    (is (play! 8 9))))

(deftest can-hit?-test
  (binding [noir.session/*noir-session* (atom {})]
    (reset-game!)
    (is (can-hit? 5 1)
    (is (can-hit? 5 0)))))

(deftest canPlay-test
  (binding [noir.session/*noir-session* (atom {})]
    (reset-game!)
    (is (play! 5 0)
    (is (play! 5 2)))))

(deftest scenario1-test
  "it should not be possible to choose a cell that is already taken"
  (binding [noir.session/*noir-session* (atom {})]
    (reset-game!)
    (play! 5 1) ;;normal white move
    (play! 4 0)
    (is (= (get-board-cell 4 0) \x))
    (play! 2 0) ;;normal black move   
    (play! 3 1)
    (is (= (get-board-cell 3 1) \o))
    
    (play! 5 3) ;;invalid white move
    (play! 2 0)
    (is (= (get-board-cell 2 0) \-))
    (play! 4 4) ;; normal white move
    (is (= (get-board-cell 4 4) \x))
    
    (play! 2 6) ;;normal black move
    (play! 3 7) 
    (is (= (get-board-cell 3 7) \o))
    
    (play! 5 5) ;; normal white move
    (play! 4 6)  
    (is (= (get-board-cell 4 6) \x))
    
    (play! 2 4) ;;try black move instead of a hit
    (play! 3 5)      
    (is (= (get-board-cell 2 4) \o))
    (is (= (get-board-cell 3 5) \-))
    
    (play! 3 7) ;;hit by black
    (play! 5 5)      
    (is (= (get-board-cell 3 7) \-))    
    (is (= (get-board-cell 4 6) \-))
    (is (= (get-board-cell 5 5) \o))
    
    ;;more hits by black
    (play! 3 3)
    (is (= (get-board-cell 5 5) \-))
    (is (= (get-board-cell 4 4) \-))
    (is (= (get-board-cell 3 3) \o))
    
    ;;some moves to make a new good test scenario
    (play! 6 6)
    (play! 5 5)
    
    (play! 1 1)
    (play! 2 0)
    
    (play! 6 2)
    (play! 5 1)
    
    (play! 3 3)
    (play! 4 4)
    
    (is (= (get-board) [[\o \  \o \  \o \  \o \ ]
                        [\  \-  \  \o  \  \o  \  \o ] 
                        [\o \  \o \  \o \  \- \ ]
                        [\  \o \  \- \  \- \  \-]
                        [\x \  \- \  \o \  \- \ ]
                        [\  \x \  \- \  \x \  \x]
                        [\x  \  \-  \  \x  \  \-  \ ]
                        [\  \x \  \x \  \x \  \x]]))
    
   (play! 5 5)
   (play! 3 3)
   (play! 1 1)
   (is (= (get-board-cell 5 5) \-))
   (is (= (get-board-cell 4 4) \-))
   (is (= (get-board-cell 3 3) \-))
   (is (= (get-board-cell 3 3) \-))
   (is (= (get-board-cell 2 2) \-))
   (is (= (get-board-cell 1 1) \x))
    
   (play! 0 0)
   (play! 2 2)
   
   (play! 7 1)
   (play! 6 2)
   (play! 2 2)
   (play! 3 3)
   
   (play! 4 0)
   (play! 2 2)
   (play! 4 4)
   
    (is (= (get-board) [[\- \  \o \  \o \  \o \ ]
                        [\  \-  \  \o  \  \o  \  \o ] 
                        [\o \  \- \  \o \  \- \ ]
                        [\  \- \  \- \  \- \  \-]
                        [\- \  \- \  \x \  \- \ ]
                        [\  \x \  \- \  \- \  \x]
                        [\x  \  \x  \  \x  \  \-  \ ]
                        [\  \- \  \x \  \x \  \x]]))   
    
    (play! 1 5)
    (play! 2 6)
    
    (play! 7 7)
    (play! 6 6)
    
    (play! 0 4)
    (play! 1 5)
    
    (play! 6 6)
    (play! 5 5)
    
    (play! 2 4)
    (play! 3 3)
    
    (play! 4 4)
    (play! 2 2)
    (play! 0 4)
    
    
   (is (= (get-board-cell 0 4) \X))

    (is (= (get-board) [[\- \  \o \  \X \  \o \ ]
                        [\  \-  \  \-  \  \o  \  \o ] 
                        [\o \  \- \  \- \  \o \ ]
                        [\  \- \  \- \  \- \  \-]
                        [\- \  \- \  \- \  \- \ ]
                        [\  \x \  \- \  \x \  \x]
                        [\x  \  \x  \  \x  \  \-  \ ]
                        [\  \- \  \x \  \x \  \-]]))  
    
    (play! 2 6)
    (play! 3 5)
    
    (play! 0 4)
    (play! 3 7)
    (is (= (get-board-cell 3 7) \X))
    
    (play! 0 2)
    (play! 1 3)
    
    (play! 3 7)
    (play! 4 6)
    
    (play! 1 7)
    (play! 2 6)
    
    (play! 4 6)
    (play! 0 2)
    
    
    
    
    (is (= (get-board-cell 4 6) \-))
    (is (= (get-board-cell 3 5) \-))
    (is (= (get-board-cell 2 4) \-))
    (is (= (get-board-cell 1 3) \-))
    (is (= (get-board-cell 0 2) \X))

    (play! 2 6)
    (play! 3 5)
    
    (play! 0 2)
    (play! 4 6)
    
    (play! 2 0)
    (play! 3 1)
    
    (play! 5 1)
    (play! 4 2)
    
    (play! 3 1)
    (play! 5 3) ;;make queen for black
    (play! 7 1)
    
    (play! 4 6) 
    (play! 3 5)
    
    (play! 7 1)
    (play! 2 6) ;;hit white queen
    
    (play! 5 5)
    (play! 4 4)
    
    (play! 2 6)
    (play! 5 3)
    
    (play! 6 4)
    (play! 4 2) ;;hit black queen
    
    (play! 0 6)
    (play! 1 5)
    
    (play! 4 2)
    (play! 3 3)
    
    (play! 1 5)
    (play! 2 4)
    
    (play! 3 3)
    (play! 1 5)
    
    (is(= (winner?) \x))
    
    
    (reset-game!)))



  