(ns mimir.test.pong
  (:use [mimir.well :only (update rule fact facts retract reset run* run-once is-not)]
        [mimir.match :only (condm truth)]
        [mimir.test.common]
        [clojure.test])
  (:require [lanterna.screen :as s]
            [lanterna.terminal :as t]))

(reset)

(def x 0)
(def y 1)

(defn bounce [axis]
  (update :speed [:speed axis] -))

(defn place-ball [width height]
  (update :ball [:ball] [(int (/ width 2)) (rand-int height)]))

(defn score [who width height]
  (place-ball width height)
  (update {:player who} [:score] inc))

(facts {:ball [0 0]}
       {:speed [1 1]})

(rule move

      {:speed [dx dy]}

      =>

      (update :ball [:ball] #(mapv + [dx dy] %)))

(rule left-wall

      {:screen [width height]}
      {:ball [0 _]}
      {:speed [neg? _]}

      =>

      (score :computer width height))

(rule right-wall

      {:screen [width height]}
      {:ball [width _]}
      {:speed [pos? _]}

      =>

      (score :human width height))


(rule floor

      {:ball [_ 0]}
      {:speed [_ neg?]}

      =>

      (bounce y))

(rule ceiling

      {:screen [_ height]}
      {:ball [_ height]}
      {:speed [_ pos?]}

      =>

      (bounce y))

(declare screen)

(def colors {:fg :white :bg :black})
(def reverse-video {:fg (:bg colors) :bg (:fg colors)})

(def paddle-size 5)
(def paddle-margin 2)
(def score-line 2)

(defn above-bottom? [y py]
  (and (> (- y paddle-size) py)))

(defn move-paddle [who direction]
  (update {:player who} [:paddle y] direction))

(rule paddle-up

      {:key :up}
      {:player :human :paddle [_ pos?]}

      =>

      (move-paddle :human dec))

(rule paddle-down

      {:key :down}
      {:screen [_ height]}
;      {:player :human :paddle [_ (partial above-bottom? height)]}
;      (< py (- height paddle-size))

      =>

      (move-paddle :human inc))

(rule exit

      {:key :escape}

      =>

      :exit)

(defn blank []
  (s/clear screen)
  (s/redraw screen)
  (let [[x y] (s/get-size screen)]
    (doseq [y (range 0 y)]
      (s/put-string screen 0 y (apply str (repeat x " ")) colors))
    (s/redraw screen)))

(defn center [total length]
  (int (- (/ total 2) (/ length 2))))

(defn centered-text [y s]
  (let [[x _] (s/get-size screen)]
    (s/put-string screen (center x (count s)) y s colors)))

(defn draw-net []
  (let [[x y] (s/get-size screen)]
    (doseq [y (range 0 y 3)]
      (s/put-string screen (int (/ x 2)) y " " reverse-video))))

(defn draw-score [x y score]
  (s/put-string screen x 2 (str score)
                (if (< y score (+ y paddle-size)) reverse-video colors)))

(defn draw-paddle [x y]
  (doseq [y (range y (+ y paddle-size))]
    (s/put-string screen x y " " reverse-video))
  (s/put-string screen x (dec y) " " colors)
  (s/put-string screen x (+ y paddle-size) " " colors))

(defn paddle [who x y]
  (update {:player who} merge {:paddle [x y] :score 0})
  (draw-paddle x y)
  (draw-score x y 0))

(defn draw-ball [x y]
  (s/move-cursor screen x y))

(defn header []
  (centered-text 0 "Welcome to Mímir Pong!")
  (centered-text 1 "Press Esc to exit"))

(defn draw-background []
  (blank)
  (draw-net)
  (header))

(defn resize-screen [x y]
  (update :screen {:screen (mapv dec [x y])})

  (draw-background)

  (place-ball x y)

  (paddle :human paddle-margin (center y paddle-size))
  (paddle :computer (- x paddle-margin) (center y paddle-size))

  (s/redraw screen))

(defn frame [events]
  (Thread/sleep 25)
  (s/redraw screen)
  (update {:key truth} [:key] (s/get-key screen))
  events)

(defn handle-event [e]
  (condm e
         {:ball [px py]} (draw-ball px py)
         {:paddle [px py] :score s} (do
                                      (draw-paddle px py)
                                      (draw-score px py s))))

(defn main [screen-type]
  (def screen (s/get-screen screen-type))
  (s/add-resize-listener screen resize-screen)

  (s/in-screen screen
               (->> (run*)
                    (mapcat frame)
                    (take-while (is-not :exit))
                    (map handle-event)
                    doall)))

(defn -main [& [screen-type _]]
  (main (read-string (or screen-type ":text")))
  nil)