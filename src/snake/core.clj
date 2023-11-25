(ns snake.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class))

(def is-paused (atom true))

(def unit-width 25)

(def snake-state (atom {:color 200
                        :previous-direction [0 0]
                        :direction [0 0]
                        :position [[0 0] [0 25] [0 50]]}))

(defn- update-food []
  [(- (* unit-width (rand-int 40)) 500) (- (* unit-width (rand-int 40)) 500)])

(defn- update-food-state [current]
  (assoc current :position (update-food)))

(def food-state (atom {:color 250 :position (update-food)}))

(defn- setup []
  (q/frame-rate 4)
  (q/color-mode :hsb)
  {:color 200
   :position [0 0]})

(defn- update-state [currentState newState]
  (let [color (:color newState)
        previous-direction (:previous-direction newState)
        direction (:direction newState)
        position (:position newState)]
    (if (not @is-paused)
      {:color color
       :previous-direction previous-direction
       :direction direction
       :position position}
      currentState)))

(defn- get-direction-from-key [key]
  (case key
    :up [0 -1]
    :down [0 1]
    :right [1 0]
    :left [-1 0]))

(defn- compute-position [position direction]
  (let [head (first position)
        tail (drop 1 position)]
    (if @is-paused
      position
      (let [result (cons [(+ (first head) (* unit-width (first direction)))
                          (+ (second head) (* unit-width (second direction)))]
                         (cons head (drop-last tail)))]
        (vec result)))))

(defn- out-of-bounds? [[x y]]
  (if (or (< x -500) (> x 500))
    true
    (if (or (< y -500) (> y 500))
      true
      false)))

(defn- grow-tail []
  (let [color (:color @snake-state)
        previous-direction (:previous-direction @snake-state)
        direction (:direction @snake-state)
        position (:position @snake-state)
        tail-end (last position)
        delta (mapv #(* (- unit-width) %) previous-direction)
        new-tail-end (vec (map-indexed #(+ (nth delta %1) %2) tail-end))
        new-position (conj position new-tail-end)]
    (println "---" new-tail-end position new-position)
    (swap! snake-state update-state {:color color
                                     :previous-direction previous-direction
                                     :direction direction
                                     :position (compute-position new-position direction)})))

(defn- update-scene []
  (let [color (:color @snake-state)
        previous-direction (:previous-direction @snake-state)
        direction (:direction @snake-state)
        position (:position @snake-state)
        head (first position)]
    (if (out-of-bounds? head)
      (swap! is-paused #(not %))
      (let [food-position (:position @food-state)]
        (if (= head food-position)
          (do (swap! food-state update-food-state)
              (grow-tail))
          (swap! snake-state update-state {:color color
                                           :previous-direction previous-direction
                                           :direction direction
                                           :position (compute-position position direction)}))))))

(defn- draw-unit [x y]
  (q/rect x y unit-width unit-width))

(defn- draw [_]
  (println "DIRECTION" (:direction @snake-state))
  (println "POSITION" (:position @snake-state))
  (update-scene)
  (q/background 0)
  (q/fill (:color @snake-state) 255 255)
  (let [position (:position @snake-state)]
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      (doseq [[a b] position] (draw-unit a b))
      (let [food-position (:position @food-state)
            x (first food-position)
            y (second food-position)]
        (q/fill (:color @food-state) 255 255)
        (draw-unit x y)))))

(defn- key-press [_ input]
  (let [pressed-key (:key input)]
    (when (and @is-paused (some #{(:key input)} [:up :down :left :right]))
      (swap! is-paused #(not %)))
    (swap! snake-state update-state {:color (:color @snake-state)
                                     :previous-direction (:direction @snake-state)
                                     :direction (get-direction-from-key pressed-key)
                                     :position (:position @snake-state)}))
  (q/redraw))

(q/defsketch snake
  :title "Snake"
  :size [1000 1000]
  :setup setup
  :draw draw
  :key-pressed key-press
  :features [:keep-on-top]
  :middleware [m/fun-mode])

(defn -main [& _]
  (q/sketch
   :title "Snake"
   :size [1000 1000]
   :setup setup
   :draw draw
   :key-pressed key-press
   :features [:keep-on-top]
   :middleware [m/fun-mode]))
