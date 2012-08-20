(ns mimir.test.pong
  (:use [mimir.well :only (update rule reset run* is-not)])
  (:require [lanterna.screen :as s]))

(reset)

(def axis {:x 0 :y 1})
(def paddle-size 5)
(defn half [n] (bit-shift-right n 1))

(rule ball-keeps-moving
      {:speed [dx dy]}
      =>
      (update :ball [:ball] #(mapv + [dx dy] %)))

(defn place-ball-at-center [width height]
  (update {:ball =} merge {:ball [(half width) (rand-int height)]}))

(defn score [who]
  (update {:player who} [:score] inc))

(rule computer-scores
      {:screen [width height]}
      {:ball [0 _]}
      =>
      (place-ball-at-center width height)
      (score :computer))

(rule player-scores
      {:screen [width height]}
      {:ball [width _]}
      =>
      (place-ball-at-center width height)
      (score :human))

(defn bounce [axis]
  (update :speed [:speed axis] -))

(rule ball-hits-paddle
      {:ball [bx by] :speed [dx _]}
      {:paddle [(+ ?dx ?bx) #(<= % ?by (+ paddle-size %))]}
      =>
      (bounce (:x axis)))

(rule ball-hits-floor
      {:ball [_ 0] :speed [_ neg?]}
      =>
      (bounce (:y axis)))

(rule ball-hits-ceiling
      {:screen [_ height]}
      {:ball [_ height] :speed [_ pos?]}
      =>
      (bounce (:y axis)))

(defn move-paddle [who direction]
  (update {:player who} [:paddle (:y axis)] direction))

(rule player-moves-paddle-up
      {:key :up}
      =>
      (move-paddle :human dec))

(rule player-moves-paddle-down
      {:key :down}
      =>
      (move-paddle :human inc))

(defn middle-of-paddle [y]
  (+ (half paddle-size) y))

(rule computer-moves-paddle-up
      {:ball [_ by]}
      {:player :computer :paddle [_ py]}
      (< ?by (middle-of-paddle ?py))
      =>
      (move-paddle :computer dec))

(rule computer-moves-paddle-down
      {:ball [_ by]}
      {:player :computer :paddle [_ py]}
      (> ?by (middle-of-paddle ?py))
      =>
      (move-paddle :computer inc))

(rule paddle-hits-ceiling
      {:player who :paddle [_ (complement pos?)]}
      =>
      (move-paddle who 0))

(rule paddle-hits-floor
      {:screen [_ height]}
      {:player who :paddle [_ py]}
      (> (+ paddle-size ?py) ?height)
      =>
      (move-paddle who (inc (- height paddle-size))))

(declare screen)

(def colors {:fg :white :bg :black})
(def reverse-video {:fg (:bg colors) :bg (:fg colors)})

(defn puts
  ([x y s] (puts x y s colors))
  ([x y s opts] (s/put-string screen x y (str s) opts)))

(rule player-exits-game
      {:key :escape}
      =>
      (s/stop screen)
      (System/exit 0))

(rule draw-ball
      {:ball [x y]}
      =>
      (s/move-cursor screen x y))

(rule draw-paddle
      {:paddle [x y]}
      =>
      (doseq [y (range y (+ y paddle-size))]
        (puts  x y " " reverse-video))
      (puts x (dec y) " ")
      (puts x (+ y paddle-size) " "))

(rule draw-score :salience 1
      {:paddle [x y] :score s}
      =>
      (puts x 2 s))

(defn blank [x y]
  (s/clear screen)
  (s/redraw screen)
  (doseq [y (range 0 y)]
    (puts 0 y (apply str (repeat x " ")))))

(defn center [total length]
  (half (- total length)))

(defn centered-text [width y s]
  (puts (center width (count s)) y s))

(defn draw-net [x y]
  (doseq [y (range 0 y 3)]
    (puts (half x) y " " reverse-video)))

(defn create-paddle [who x y]
  (update {:player who} merge {:paddle [x y] :score 0}))

(defn header [width]
  (centered-text width 0 "Welcome to Mímir Pong!")
  (centered-text width 1 "Press Esc to exit"))

(defn draw-background [x y]
  (blank x y)
  (draw-net x y)
  (header x))

(defn start-game [x y]
  (place-ball-at-center x y)
  (update :ball merge {:speed [1 1]})
  (create-paddle :human 2 (center y paddle-size))
  (create-paddle :computer (- x 2) (center y paddle-size)))

(defn resize-screen [x y]
  (update :screen {:screen (mapv dec [x y])})
  (draw-background x y)
  (start-game x y))

(defn frame []
  (s/redraw screen)
  (Thread/sleep 20)
  (update :key {:key (->> (repeatedly #(s/get-key screen))
                          (take-while identity) last)}))

(defn -main [& [screen-type _]]
  (def screen (s/get-screen (read-string (or screen-type ":text"))))
  (s/add-resize-listener screen resize-screen)
  (s/in-screen screen (dorun (interleave (run*) (repeatedly frame)))))
