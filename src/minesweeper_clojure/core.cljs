 (ns ^:figwheel-hooks minesweeper-clojure.core
   (:require
    [clojure.string :refer [join]]
    [goog.dom :as gdom]
    [minesweeper-clojure.svgs :refer [svg]]
    [reagent.core :as reagent :refer [atom create-class]]
    [reagent.dom :as rdom]))

;; state creation helpers

(defn gen-mines
  "Create a vec of 'mines', ex. [0 0 1 0] for a 2x2 grid with '4' difficulty (viz. '1 in 4')"
  [x-dim y-dim difficulty]
  (let [num-squares (* x-dim y-dim)
        num-mines (max (quot num-squares difficulty) 1)] ;; make sure there's at least 1 mine
    (->> (concat (repeat num-mines 1) (repeat (- num-squares num-mines) 0)) ;; => ex. [1 0 0 0]
         shuffle))) ;; => ex. [0 0 1 0]

(defn edges
  "Given an index and dims, determine if/where it's on the board's border."
  [i x-dim y-dim]
  {:is-left-edge (zero? (mod i x-dim))
   :is-right-edge (zero? (mod (inc i) x-dim))
   :is-top-edge (< i x-dim)
   :is-bottom-edge (>= i (- (* x-dim y-dim) x-dim))})

(defn adjacent-indices
  "Given an index and dims, return a vec of adjacent indices."
  [i x-dim y-dim]
  (let [{:keys [is-left-edge is-right-edge is-top-edge is-bottom-edge]} (edges i x-dim y-dim)
        left-i (when (not is-left-edge) (dec i))
        right-i (when (not is-right-edge) (inc i))
        top-i (when (not is-top-edge) (- i x-dim))
        bottom-i (when (not is-bottom-edge) (+ i x-dim))
        top-left-i (when (and (not is-left-edge) (not is-top-edge)) (- (dec i) x-dim))
        top-right-i (when (and (not is-right-edge) (not is-top-edge)) (- (inc i) x-dim))
        bottom-left-i (when (and (not is-left-edge) (not is-bottom-edge)) (+ (dec i) x-dim))
        bottom-right-i (when (and (not is-right-edge) (not is-bottom-edge)) (+ (inc i) x-dim))]
    (->> [left-i right-i top-i bottom-i top-left-i top-right-i bottom-left-i bottom-right-i]
         (filter #(not (nil? %))))))

(defn num-adjacent-mines
  "Given an index, mines, and dims, determine the number of adjacent mines."
  [i mines x-dim y-dim]
  (->> (adjacent-indices i x-dim y-dim)
       (map #(nth mines %))
       (reduce +)))

(defn gen-board
  "Create a vec of square maps,
  e.g. [{:i 0 :is-flag false :is-mine true :is-revealed false :num-adjacent-mines 1} ... ]"
  [x-dim y-dim difficulty]
  (let [mines-vec (gen-mines x-dim y-dim difficulty)]
    (->> (map-indexed
          (fn [i x] {:is-flag false
                     :is-mine x
                     :is-mistake false
                     :num-adjacent-mines (num-adjacent-mines i mines-vec x-dim y-dim)
                     :is-revealed false})
          mines-vec)
         vec)))

;; state

(def has-initially-loaded (atom false))
(def dims (atom {:x-dim 10 :y-dim 10}))
(def difficulty (atom 10)) ;; there will be 1 mine per <difficulty> squares; TODO change to number-of-mines-total
(def board (atom (gen-board (:x-dim @dims) (:y-dim @dims) @difficulty)))
(def is-game-active (atom true))

(defn toggle-flag!
  "Add or remove a flag on a square."
  [i]
  (swap! board update-in [i :is-flag] not))

(defn start-game!
  "Reset board and permit gameplay."
  []
  (do (reset! is-game-active true)
      (reset! board (gen-board (:x-dim @dims) (:y-dim @dims) @difficulty))))

;; TODO add timer
;; TODO add happy face

(defn game-over!
  "Reveal mines, end gameplay."
  [i]
  ;; TODO reveal all mines
  ;; TODO show incorrect flags
  (do
    (swap! board assoc-in [i :is-mistake] true)
    (reset! is-game-active false)))

(defn reveal!
  "Expose contents, then conditionally (1) end game or
  (2) recursively reveal adjacent non-mine squares."
  [i]
  (let [x-dim (:x-dim @dims) y-dim (:y-dim @dims)
        {:keys [is-mine num-adjacent-mines]} (@board i)
        is-blank (and (zero? is-mine) (zero? num-adjacent-mines))]
    (do
      (swap! board assoc-in [i :is-revealed] true)
      (cond
        ;; (1) end game
        (pos? is-mine) (game-over! i)
        ;; (2) recursively reveal adjacent non-mine squares
        is-blank (doseq [x (adjacent-indices i x-dim y-dim)]
                   (let [{:keys [is-revealed is-mine num-adjacent-mines]} (@board x)]
                     (when (false? is-revealed) (reveal! x))))))))

(defn main []
  (letfn [(keyboard-listeners [e]
            (let [is-r (= (.-keyCode e) 82)]
              (when is-r (start-game!))))]
    (create-class
     {:component-did-mount
      (fn [] (do
               (.addEventListener js/window "contextmenu" #(.preventDefault %))
               (.addEventListener js/document "keydown" keyboard-listeners)
               (js/setTimeout #(reset! has-initially-loaded true) 0)))
      :reagent-render
      (fn [this]
        [:div.main {:class (if @has-initially-loaded "has-initially-loaded")}
         [:div.board-container
          [:div.board.constrain-width
           [:div.board-inner {:style {:grid-template-columns (join " " (repeat (:x-dim @dims) "1fr"))}
                              ;; :on-click #(when (not @is-game-active) (start-game!))
                              }
            (doall (map-indexed
                    (fn [i {:keys [is-flag is-mine is-mistake is-revealed num-adjacent-mines]}]
                      ^{:key (str i)}
                      [:a.square
                       {:class [(when (not @is-game-active) "pointer-events-none")
                                (when (pos? is-mine) "is-mine")
                                (when is-mistake "is-mistake")
                                (when is-revealed "is-revealed")
                                (when is-flag "is-flag")]
                        :on-click #(reveal! i)
                        :on-context-menu #(toggle-flag! i)}
                       [:span.square-inner
                        (cond is-revealed (if (not (zero? is-mine)) (svg 'mine)
                                              (when (not (zero? num-adjacent-mines)) num-adjacent-mines))
                              is-flag (svg 'flag))]])
                    @board))
            [:div.board-horizontal-lines " "]
            [:div.board-vertical-lines " "]]]]])})))

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
