(ns snake.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [dynne.sampled-sound :refer [sinusoid
                                         play
                                         ->stereo
                                         pan]]) 
  (:gen-class))

(def is-paused (atom true))

(def unit-width 25)

(def snake-state (atom {:color 200
                        :previous-direction [0 0]
                        :direction [0 0]
                        :position [[0 0]]
                        :speed 4}))

(def base-frequency (/ 0.4 (:speed @snake-state)))

(def low-beep (sinusoid base-frequency 220))
(def high-beep (sinusoid base-frequency 660))
(def beep (-> (->stereo low-beep high-beep)
              (pan 0.7)))

(def l (-> (sinusoid (* 3 base-frequency) 440)))
(def r (-> (sinusoid (* 3 base-frequency) 880)))
(def eat-sound
  (-> (->stereo l r)
      (pan 0.7)))

(defn- print-score []
  (q/fill 255)
  (q/text-size 10)
  (q/text-font (q/create-font "Arial-Black" 30 true))
  (q/text (str "Score: " (dec (count (:position @snake-state)))) 450 25))

(defn- update-food []
  [(- (* unit-width (rand-int 40)) 500) (- (* unit-width (rand-int 40)) 500)])

(defn- update-food-state [current]
  (assoc current :position (update-food)))

(def food-state (atom {:color 250 :position (update-food)}))

(defn- setup []
  (q/frame-rate (:speed @snake-state))
  (q/color-mode :hsb)
  {:color 200
   :position [0 0]})

(defn- update-state [currentState newState]
  (let [color (:color newState)
        previous-direction (:previous-direction newState)
        direction (:direction newState)
        position (:position newState)
        speed (:speed newState)]
    (if (not @is-paused)
      (do (play beep)
          {:color color
           :previous-direction previous-direction
           :direction direction
           :position position
           :speed speed})
      currentState)))

(defn- get-direction-from-key [key]
  (case key
    :up [0 -1]
    :down [0 1]
    :right [1 0]
    :left [-1 0]))

(defn- compute-position [position direction]
  (let [head (first position)
        tail (drop 1 position)
        new-head-position [(+ (first head) (* unit-width (first direction)))
                           (+ (second head) (* unit-width (second direction)))]]
    (if @is-paused
      position
      (if (> (count position) 1) 
        (let [result (cons new-head-position
                           (cons head (drop-last tail)))]
          (vec result))
        [new-head-position]))))

(defn- out-of-bounds? [[x y]]
  (if (or (< x -500) (> x 500))
    true
    (if (or (< y -500) (> y 500))
      true
      false)))

(defn- grow-tail []
  (play eat-sound)
  (let [color (:color @snake-state)
        previous-direction (:previous-direction @snake-state)
        direction (:direction @snake-state)
        position (:position @snake-state)
        speed (:speed @snake-state)
        tail-end (last position)
        delta (mapv #(* (- unit-width) %) previous-direction)
        new-tail-end (vec (map-indexed #(+ (nth delta %1) %2) tail-end))
        new-position (conj position new-tail-end)]
    (swap! snake-state update-state {:color color
                                     :previous-direction previous-direction
                                     :direction direction
                                     :position (compute-position new-position direction)
                                     :speed (inc speed)})))

(defn- update-scene []
  (let [color (:color @snake-state)
        previous-direction (:previous-direction @snake-state)
        direction (:direction @snake-state)
        position (:position @snake-state)
        speed (:speed @snake-state)
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
                                           :position (compute-position position direction)
                                           :speed speed}))))))

(defn- draw-unit [x y]
  (q/rect x y unit-width unit-width))

(defn- draw-food []
  (let [food-position (:position @food-state)
        x (first food-position)
        y (second food-position)]
    (q/fill 255 255 255)
    (draw-unit x y)))

(defn- draw-snake [body]
  (doseq [[a b] body] (draw-unit a b)))

(defn- draw [_]
  (q/frame-rate (:speed @snake-state))
  (update-scene)
  (q/background 0) 
  (q/fill (:color @snake-state) 255 255)
  (let [position (:position @snake-state)]
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      (draw-snake position)
      (draw-food)))
  (print-score))

(defn- key-press [_ input]
  (let [pressed-key (:key input)]
    (when (and @is-paused (some #{(:key input)} [:up :down :left :right]))
      (swap! is-paused #(not %)))
    (swap! snake-state update-state {:color (:color @snake-state)
                                     :previous-direction (:direction @snake-state)
                                     :direction (get-direction-from-key pressed-key)
                                     :position (:position @snake-state)
                                     :speed (:speed @snake-state)}))
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
