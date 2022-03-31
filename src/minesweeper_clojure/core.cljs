(ns ^:figwheel-hooks minesweeper-clojure.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]))

(def dims (atom {:x-dim 4 :y-dim 4}))
(def difficulty (atom 6)) ;; there will be 1 bomb per <difficulty> squares

(defn gen-bombs
  "Create a vec of 'bombs', eg. [0 0 1 0] for a 2x2 grid with '4' difficulty (viz. '1 in 4')"
  [x-dim y-dim difficulty]
  (let [num-squares (* x-dim y-dim)
        num-bombs (quot num-squares difficulty)]
    (->> (concat (repeat num-bombs 1) (repeat (- num-squares num-bombs) 0))
         shuffle)))

(defn num-adjacent-bombs
  "Given an index, bombs, and dims, determine the number of adjacent bombs."
  [i bombs x-dim y-dim]
  (let [is-left-edge (zero? (mod i x-dim))
        is-right-edge (zero? (mod (inc i) x-dim))
        is-top-edge (< i x-dim)
        is-bottom-edge (>= i (- (* x-dim y-dim) x-dim))
        left (if (not is-left-edge) (nth bombs (dec i)) 0)
        right (if (not is-right-edge) (nth bombs (inc i)) 0)
        top (if (not is-top-edge) (nth bombs (- i x-dim)) 0)
        bottom (if (not is-bottom-edge) (nth bombs (+ i x-dim)) 0)
        top-left (if (and (not is-left-edge) (not is-top-edge)) (nth bombs (- (dec i) x-dim)) 0)
        top-right (if (and (not is-right-edge) (not is-top-edge)) (nth bombs (- (inc i) x-dim)) 0)
        bottom-left (if (and (not is-left-edge) (not is-bottom-edge)) (nth bombs (+ (dec i) x-dim)) 0)
        bottom-right (if (and (not is-right-edge) (not is-bottom-edge)) (nth bombs (+ (inc i) x-dim)) 0)
        adjacent-bombs [top-left top top-right left right bottom-left bottom bottom-right]]
    ;; (do (println [top-left top top-right])
    ;;     (println [left (nth bs i) right])
    ;;     (println [bottom-left bottom bottom-right]))
    (reduce + adjacent-bombs)))

(defn gen-board
  "Create a vec of square maps, eg [{:i 0 :is-bomb true :is-revealed false :num-adjacent-bombs 1} ... ]"
  [x-dim y-dim difficulty]
  (let [bombs-vec (gen-bombs x-dim y-dim difficulty)]
    (->> (map-indexed
          (fn [i x] {:is-bomb x :num-adjacent-bombs (num-adjacent-bombs i bombs-vec x-dim y-dim) :is-revealed false})
          bombs-vec)
         vec)))

(defn hello-world []
  [:div "hi"])

(defn get-app-element []
  (gdom/getElement "app"))
(defn mount [el]
  (rdom/render [hello-world] el))
(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))
(mount-app-element)
(defn ^:after-load on-reload []
  (mount-app-element))
