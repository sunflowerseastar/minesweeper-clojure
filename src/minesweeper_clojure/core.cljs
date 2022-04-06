 (ns ^:figwheel-hooks minesweeper-clojure.core
   (:require
    [clojure.string :refer [join]]
    [goog.dom :as gdom]
    [minesweeper-clojure.svgs :refer [svg]]
    [reagent.core :as reagent :refer [atom create-class]]
    [reagent.dom :as rdom]))

;; state creation helpers

(defn gen-mines
  "Create a vec of 'mines', ex. [false false true false] for a 2x2 grid with num-mines-total = 1"
  [x-dim y-dim num-mines-total]
  (let [num-squares (* x-dim y-dim)]
    (->> (concat (repeat num-mines-total true) (repeat (- num-squares num-mines-total) false))
         shuffle)))

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

(defn bool->int
  "Helper to convert false -> 0 and true -> 0."
  [b] (get {false 0 true 1} b))

(defn num-adjacent-mines
  "Given an index, mines, and dims, determine the number of adjacent mines."
  [i mines x-dim y-dim]
  (->> (adjacent-indices i x-dim y-dim)
       (map #(nth mines %))
       (map bool->int)
       (reduce +)))

(defn gen-board
  "Create a vec of square maps,
  e.g. [{:i 0 :is-flag false :is-mine true :is-revealed false :num-adjacent-mines 1} ... ]"
  [x-dim y-dim num-mines-total]
  (let [mines-vec (gen-mines x-dim y-dim num-mines-total)]
    (->> (map-indexed
          (fn [i x] {:is-final false
                     :is-flag false
                     :is-mine x
                     :is-mistake false
                     :is-revealed false
                     :num-adjacent-mines (num-adjacent-mines i mines-vec x-dim y-dim)})
          mines-vec)
         vec)))

;; state

(def has-initially-loaded (atom false))
(def dims (atom {:x-dim 20 :y-dim 16}))
(def num-mines-total (atom 40))
;; (def dims (atom {:x-dim 2 :y-dim 2}))
;; (def num-mines-total (atom 3))
(def board (atom (gen-board (:x-dim @dims) (:y-dim @dims) @num-mines-total)))
(def gameplay-state (atom 'active)) ;; active | win | lose
(defonce tick-interval (atom 0))
(defonce seconds-elapsed (atom 0))

(defn tick! []
  (swap! seconds-elapsed inc))

(defn start-tick-interval! []
  (reset! tick-interval (js/setInterval #(tick!) 1000)))

(defn toggle-flag!
  "Add or remove a flag on a square."
  [i]
  (swap! board update-in [i :is-flag] not))

(defn start-game!
  "Recreate randomized-mines board based on UI state, (2) reset gameplay
  state, (3) reset the count-up timer's state to 0, (4) restart the JS timer
  that triggers the timer."
  []
  (do
    ;; (1) generate a new board
    (reset! board (gen-board (:x-dim @dims) (:y-dim @dims) @num-mines-total))
    ;; (2) permit gameplay
    (reset! gameplay-state 'active)
    ;; (3) put the timer back at 0
    (reset! seconds-elapsed 0)
    ;; (4) restart the JS interval that controls the count-up timer
    (start-tick-interval!)))

(defn game-over-lose!
  "(1) Mark game-ending mine as 'final' (red), (2) mark incorrect flags as
  'mistake' (X), (3) reveal mines, (4) end gameplay state, (5) stop timer."
  [i]
  (do
    ;; (1) make the game-ending square turn red (background, via class -> css)
    (swap! board assoc-in [i :is-final] true)
    ;; (2) mark incorrect flags
    (doseq [flag-i (->> @board (keep-indexed #(if (and (:is-flag %2) (not (:is-mine %2))) %1)))]
      (swap! board assoc-in [flag-i :is-mistake] true))
    ;; (3) reveal all of the mines on the board
    (doseq [mine-i (->> @board (keep-indexed #(if (and (:is-mine %2) (not (:is-flag %2))) %1)))]
      (swap! board assoc-in [mine-i :is-revealed] true))
    ;; (4) end gameplay
    (reset! gameplay-state 'lose)
    ;; (5) clear JS tick-interval that triggers count-up timer
    (js/clearInterval @tick-interval)))

(defn game-over-win! []
  (do (println "win =)") (reset! gameplay-state 'win)
      (js/clearInterval @tick-interval)))

(defn reveal!
  "(1) Expose contents, then conditionally (2) end game due to mine, (3) end game
  due to a win, or (4) recursively reveal adjacent non-mine squares."
  [i]
  (let [x-dim (:x-dim @dims) y-dim (:y-dim @dims)
        {:keys [is-mine num-adjacent-mines]} (@board i)
        is-blank (and (false? is-mine) (zero? num-adjacent-mines))
        num-squares-remaining (->> @board (keep-indexed #(if (not (:is-revealed %2)) %1)) count dec)]
    (do
      ;; (1) show what's in the square
      (swap! board assoc-in [i :is-revealed] true)
      (cond
        ;; (2) end game, lose
        is-mine (game-over-lose! i)
        ;; (3) end game, win
        (= num-squares-remaining @num-mines-total) (game-over-win!)
        ;; (4) recursively reveal adjacent non-mine squares
        is-blank (doseq [adjacent-i (adjacent-indices i x-dim y-dim)]
                   (let [{:keys [is-revealed is-mine num-adjacent-mines]} (@board adjacent-i)]
                     (when (false? is-revealed) (reveal! adjacent-i))))))))

(defn main []
  (letfn [(keyboard-listeners [e]
            (let [is-r (= (.-keyCode e) 82)]
              (when is-r (start-game!))))]
    (create-class
     {:component-did-mount
      (fn [] (do
               (.addEventListener js/window "contextmenu" #(.preventDefault %))
               (.addEventListener js/document "keydown" keyboard-listeners)
               (start-tick-interval!)
               (js/setTimeout #(reset! has-initially-loaded true) 0)))
      :reagent-render
      (fn [this]
        [:div.main {:class (if @has-initially-loaded "has-initially-loaded")}
         [:div.board-container
          [:div.above-board.constrain-width
           [:div.left
            [:span (max (- @num-mines-total (->> @board (filter :is-flag) count)) 0)]]
           [:div.center
            [:span {:on-click #(when (not (= @gameplay-state 'active)) (start-game!))}
             (cond (= @gameplay-state 'win) "B)"
                   (= @gameplay-state 'lose) "=("
                   :else "=)")]]
           [:div.right
            [:span @seconds-elapsed]]]
          [:div.board.constrain-width
           [:div.board-inner {:style {:grid-template-columns (join " " (repeat (:x-dim @dims) "1fr"))}}
            (doall (map-indexed
                    (fn [i {:keys [is-final is-flag is-mine is-mistake is-revealed num-adjacent-mines]}]
                      ^{:key (str i)}
                      [:a.square
                       {:class [(when (not= @gameplay-state 'active) "pointer-events-none")
                                (when is-final "is-final")
                                (when is-flag "is-flag")
                                (when is-mine "is-mine")
                                (when is-mistake "is-mistake")
                                (when is-revealed "is-revealed")]
                        :on-click #(reveal! i)
                        :on-context-menu #(toggle-flag! i)}
                       [:span.square-inner
                        (cond is-revealed (if is-mine (svg 'mine)
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
