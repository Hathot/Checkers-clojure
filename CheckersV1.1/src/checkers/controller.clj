(ns checkers.controller
  (:use compojure.core)
  (:require [compojure.core :as compojure]
            [checkers.view :as view]
            [checkers.model :as model]))

(defn start-page []
  (model/reset-game!)
  (view/play-screen))

(defn turn-page [button-pressed]
  (let [button-id (name (first (keys button-pressed)))
        rownr (Integer/parseInt (str (second button-id)))
        colnr (Integer/parseInt (str (nth button-id 2)))]
    (model/play! rownr colnr)
    (if-let [winner (model/winner?)]
      (view/winner-screen winner)
      
        (view/play-screen))))

(defroutes checkers-routes
  (GET "/" [] (start-page))
  (POST "/" {button-pressed :params} (turn-page button-pressed)))
