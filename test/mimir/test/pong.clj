(ns mimir.test.pong
  (:use [mimir.well :only (update rule fact facts reset run* is-not)]
        [mimir.match :only (condm)])
  (:require [lanterna.screen :as s]))

(reset)

(def axis {:x 0 :y 1})
(def paddle-size 5)

(rule move-ball
      {:speed [dx dy]}
      =>
      (update :ball [:ball] #(mapv + [dx dy] %)))

(defn place-ball [width height]
  (update :ball {:ball [(int (/ width 2)) (rand-int height)]}))

(defn score [who]
  (update {:player who} [:score] inc))

(rule left-wall
      {:screen [width height]}
      {:ball [0 _]}
      {:speed [neg? _]}
      =>
      (place-ball width height)
      (score :computer))

(rule right-wall
      {:screen [width height]}
      {:ball [width _]}
      {:speed [pos? _]}
      =>
      (place-ball width height)
      (score :human))


(defn bounce [axis]
  (update :speed [:speed axis] -))

(rule paddle
      {:ball [bx by]}
      {:speed [dx _]}
      {:paddle [(+ ?dx ?bx) #(<= % ?by (+ paddle-size %))]}
      =>
      (bounce (:x axis)))

(rule floor
      {:ball [_ 0]}
      {:speed [_ neg?]}
      =>
      (bounce (:y axis)))

(rule ceiling
      {:screen [_ height]}
      {:ball [_ height]}
      {:speed [_ pos?]}
      =>
      (bounce (:y axis)))

(defn move-paddle [who direction]
  (update {:player who} [:paddle (:y axis)] direction))

(rule paddle-up
      {:key :up}
      {:player :human :paddle [_ pos?]}
      =>
      (move-paddle :human dec))

(rule paddle-down
      {:key :down}
      {:screen [_ height]}
      {:player :human :paddle [_ py]}
      (<= (+ paddle-size ?py) ?height)
      =>
      (move-paddle :human inc))

(defn middle-of-paddle [y]
  (+ (int (/ paddle-size 2)) y))

(rule paddle-up-ai
      {:ball [_ by]}
      {:player :computer :paddle [_ py]}
      (< 0 ?by (middle-of-paddle ?py))
      =>
      (move-paddle :computer dec))

(rule paddle-down-ai
      {:ball [_ by]}
      {:screen [_ height]}
      {:player :computer :paddle [_ py]}
      (> ?by (middle-of-paddle ?py))
      (<= (+ paddle-size ?py) ?height)
      =>
      (move-paddle :computer inc))

(rule exit
      {:key :escape}
      =>
      :exit)

(declare screen)

(def colors {:fg :white :bg :black})
(def reverse-video {:fg (:bg colors) :bg (:fg colors)})

(defn puts
  ([x y s] (puts x y s colors))
  ([x y s opts] (s/put-string screen x y s opts)))

(defn blank []
  (s/clear screen)
  (s/redraw screen)
  (let [[x y] (s/get-size screen)]
    (doseq [y (range 0 y)]
      (puts 0 y (apply str (repeat x " "))))))

(defn center [total length]
  (int (- (/ total 2) (/ length 2))))

(defn centered-text [y s]
  (let [[x _] (s/get-size screen)]
    (puts (center x (count s)) y s)))

(defn draw-net []
  (let [[x y] (s/get-size screen)]
    (doseq [y (range 0 y 3)]
      (puts (int (/ x 2)) y " " reverse-video))))

(defn draw-score [x y score]
  (puts x 2 (str score)
        (if (<= y 2 (+ y paddle-size)) reverse-video colors)))

(defn draw-paddle [x y]
  (doseq [y (range y (+ y paddle-size))]
    (puts  x y " " reverse-video))
  (puts x (dec y) " ")
  (puts x (+ y paddle-size) " "))

(defn create-paddle [who x y]
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

(defn start-game [x y]
  (place-ball x y)
  (update :speed {:speed [1 1]})
  (create-paddle :human 2 (center y paddle-size))
  (create-paddle :computer (- x 2) (center y paddle-size)))

(defn resize-screen [x y]
  (update :screen {:screen (mapv dec [x y])})
  (draw-background)
  (start-game x y))

(defn frame [events]
  (s/redraw screen)
  (Thread/sleep 20)
  (update :key {:key (->> (repeatedly #(s/get-key screen))
                          (take-while identity)
                          last)})
  events)

(defn handle-event [e]
  (condm e
         {:ball [px py]} (draw-ball px py)
         {:paddle [px py] :score s} (do (draw-paddle px py)
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
