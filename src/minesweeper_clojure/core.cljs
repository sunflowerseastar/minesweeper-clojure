(ns ^:figwheel-hooks minesweeper-clojure.core
  (:require
   [clojure.string :refer [join]]
   [goog.dom :as gdom]
   [minesweeper-clojure.svgs :refer [svg]]
   [reagent.core :as reagent :refer [atom create-class]]
   [reagent.dom :as rdom]))

;; state creation helpers

(defn gen-mines
  "Create a vec of 'mines', eg. [0 0 1 0] for a 2x2 grid with '4' difficulty (viz. '1 in 4')"
  [x-dim y-dim difficulty]
  (let [num-squares (* x-dim y-dim)
        num-mines (quot num-squares difficulty)]
    (->> (concat (repeat num-mines 1) (repeat (- num-squares num-mines) 0))
         shuffle)))

(defn edges
  "Given an index and dims, determine if/where it's on the board's border."
  [i x-dim y-dim]
  {:is-left-edge (zero? (mod i x-dim))
   :is-right-edge (zero? (mod (inc i) x-dim))
   :is-top-edge (< i x-dim)
   :is-bottom-edge (>= i (- (* x-dim y-dim) x-dim))})

(defn neighbor-indices
  "Given an index and dims, return the immediate neighbor indices."
  [i x-dim y-dim]
  (let [{:keys [is-left-edge is-right-edge is-top-edge is-bottom-edge]} (edges i x-dim y-dim)]
    {:left-i (if (not is-left-edge) (dec i) nil)
     :right-i (if (not is-right-edge) (inc i) nil)
     :top-i (if (not is-top-edge) (- i x-dim) nil)
     :bottom-i (if (not is-bottom-edge) (+ i x-dim) nil)}))

(defn adjacents
  "Given an index, mines, and dims, determine the number of adjacent mines."
  [i mines x-dim y-dim]
  (let [{:keys [is-left-edge is-right-edge is-top-edge is-bottom-edge]} (edges i x-dim y-dim)
        {:keys [left-i right-i top-i bottom-i]} (neighbor-indices i x-dim y-dim)
        left (if (not is-left-edge) (nth mines left-i) nil)
        right (if (not is-right-edge) (nth mines right-i) nil)
        top (if (not is-top-edge) (nth mines top-i) nil)
        bottom (if (not is-bottom-edge) (nth mines bottom-i) nil)
        top-left (if (and (not is-left-edge) (not is-top-edge)) (nth mines (- (dec i) x-dim)) nil)
        top-right (if (and (not is-right-edge) (not is-top-edge)) (nth mines (- (inc i) x-dim)) nil)
        bottom-left (if (and (not is-left-edge) (not is-bottom-edge)) (nth mines (+ (dec i) x-dim)) nil)
        bottom-right (if (and (not is-right-edge) (not is-bottom-edge)) (nth mines (+ (inc i) x-dim)) nil)]
    [top-left top top-right left right bottom-left bottom bottom-right]))

(defn num-adjacent-mines
  "Given an index, mines, and dims, determine the number of adjacent mines."
  [i mines x-dim y-dim]
  (let [adjacent-mines (adjacents i mines x-dim y-dim)]
    (reduce + adjacent-mines)))

(defn gen-board
  "Create a vec of square maps, eg [{:i 0 :is-mine true :is-revealed false :num-adjacent-mines 1} ... ]"
  [x-dim y-dim difficulty]
  (let [mines-vec (gen-mines x-dim y-dim difficulty)]
    (do
      (println "bv: " mines-vec)
      (->> (map-indexed
            (fn [i x] {:is-mine x :num-adjacent-mines (num-adjacent-mines i mines-vec x-dim y-dim) :is-revealed false})
            mines-vec)
           vec))))

;; state

(def has-initially-loaded (atom false))
(def dims (atom {:x-dim 10 :y-dim 10}))
(def difficulty (atom 6)) ;; there will be 1 mine per <difficulty> squares
(def board (atom (gen-board (:x-dim @dims) (:y-dim @dims) @difficulty)))

(defn reveal! [i]
  (let [x-dim (:x-dim @dims) y-dim (:y-dim @dims)
        {:keys [left-i right-i top-i bottom-i]} (neighbor-indices i x-dim y-dim)
        {:keys [is-mine num-adjacent-mines]} (@board i)]
    (do
      (swap! board assoc-in [i :is-revealed] true)
      (when (and (zero? is-mine) (zero? num-adjacent-mines))
        (when (not (nil? left-i))
          (let [{:keys [is-revealed is-mine num-adjacent-mines]} (@board left-i)]
            (when (and (zero? is-mine) (false? is-revealed) (zero? num-adjacent-mines)) (reveal! left-i))))
        (when (not (nil? right-i))
          (let [{:keys [is-revealed is-mine num-adjacent-mines]} (@board right-i)]
            (when (and (zero? is-mine) (false? is-revealed) (zero? num-adjacent-mines)) (reveal! right-i))))
        (when (not (nil? top-i))
          (let [{:keys [is-revealed is-mine num-adjacent-mines]} (@board top-i)]
            (when (and (zero? is-mine) (false? is-revealed) (zero? num-adjacent-mines)) (reveal! top-i))))
        (when (not (nil? bottom-i))
          (let [{:keys [is-revealed is-mine num-adjacent-mines]} (@board bottom-i)]
            (when (and (zero? is-mine) (false? is-revealed) (zero? num-adjacent-mines)) (reveal! bottom-i))))))))

(defn main []
  (create-class
   {:component-did-mount
    (fn [] (js/setTimeout #(reset! has-initially-loaded true) 0))
    :reagent-render
    (fn [this]
      [:div.main {:class (if @has-initially-loaded "has-initially-loaded")}
       [:div.board-container
        [:div.board.constrain-width
         [:div.board-inner {:style {:grid-template-columns (join " " (repeat (:x-dim @dims) "1fr"))}}
          (map-indexed
           (fn [i {:keys [is-mine num-adjacent-mines is-revealed]}]
             ^{:key (str i)}
             [:a.square
              {:class (when is-revealed "is-revealed")
               :on-click #(reveal! i)}
              [:span.square-inner
               (when is-revealed (if (not (zero? is-mine)) (svg 'mine)
                                     (when (not (zero? num-adjacent-mines)) num-adjacent-mines)))]])
           @board)
          [:div.board-horizontal-lines " "]
          [:div.board-vertical-lines " "]]]]])}))

(defn get-app-element []
  (gdom/getElement "app"))
(defn mount [el]
  (rdom/render [main] el))
(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))
(mount-app-element)
(defn ^:after-load on-reload []
  (mount-app-element))
