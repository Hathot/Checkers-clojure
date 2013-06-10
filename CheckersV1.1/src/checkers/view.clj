(ns checkers.view
  (:use hiccup.form
        [hiccup.def :only [defhtml]]
        [hiccup.element :only [link-to]]
        [hiccup.page :only [html5 include-css include-js]])
  (:require [checkers.model :as model]))

(defhtml layout [& content]
  (html5
   [:head
    [:title "Welcome to checkers-luminus"]
    (include-css "/css/checkers.css")]
   [:body [:div#wrapper content](include-js "/js/turn.js")]))

(defn cell-html [rownum colnum cell with-submit?] 
  [:td 
   
   [:input {:id (str "b" rownum colnum)
            :name (str "b" rownum colnum) 
            :value (str cell)
            :src (str (getIcon cell rownum colnum))
            :type (str "image")
            ;:type (if with-submit? 
            ;        "submit" 
            ;        "button")
            }]])
  
(defn row-html [rownum row with-submit?]
  [:tr (map-indexed (fn [colnum cell]
                      (cell-html rownum colnum cell with-submit?))
                    row)])

(defn getIcon [cell row col](
    if(and (= row (checkers.model/select-row))
           (= col (checkers.model/select-col)))
     (if (or (= cell \o)(= cell \x))
       (str "/img/stone-selected.jpg")
       (if (or (= cell \O) (= cell \X))
         (str "/img/stone-selected-queen.jpg")   )) 
         (if(= cell \x)
           (str "/img/stone-white.jpg")
           (if(= cell \o)
             (str "/img/stone-black.jpg")
             (if(= cell \-)
               (str "/img/brown.jpg")
               (if(= cell \X)
                 (str "/img/stone-white-queen.jpg")
                 (if(= cell \O)
                   (str "/img/stone-black-queen.jpg")
                   (str "/img/lbrown.jpg"))))))))

(defn board-html [board with-submit?]
  (form-to [:post "/"]
           [:table {:id (str "board")}
            (map-indexed (fn [rownum row]
                           (row-html rownum row with-submit?)) 
                         board)]))

(defn get-color [character](
    if(= character \x)
      (str "White")
      (str "Black")
 ))

(defn play-screen []
  (layout
    [:div 
     [:p (get-color (model/get-player)) " Player, it's your turn!" ]
     (board-html (model/get-board) true)
     [:p (model/get-message)]
     ]))

(defn winner-screen [winner]
  (layout
    [:div 
   [:p "The winner is: " (get-color winner)]
   (board-html (model/get-board) false)
   (link-to "/" "Reset")]))

(defn draw-screen []
  (layout
    [:div
     [:p "It's a draw!"]
     (board-html (model/get-board) false)
     (link-to "/" "Reset")]))


  
  
