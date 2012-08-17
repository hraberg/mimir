(ns mimir.test.bounce
  (:use [mimir.well :only (update rule fact facts reset run*)]
        [mimir.test.common]
        [clojure.test]))

(def x 0)
(def y 1)

(reset)

(facts {:ball [5 5]}
       {:speed [-1 1]})

(rule move

      {:speed [dx dy]}

      =>

      (update :ball [:ball] #(mapv + [dx dy] %)))

(rule left-wall

      {:ball [1 _]}
      {:speed [neg? _]}

      =>

      (update :speed [:speed x] -))

(rule right-wall

      {:ball [9 _]}
      {:speed [pos? _]}

      =>

      (update :speed [:speed x] -))


(rule floor

      {:ball [_ 1]}
      {:speed [_ neg?]}

      =>

      (update :speed [:speed y] -))

(rule ceiling

      {:ball [_ 9]}
      {:speed [_ pos?]}

      =>

      (update :speed [:speed y] -))

(defn bounce []
  (take 10 (run*)))