(ns minesweeper-clojure.core-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [minesweeper-clojure.core :as core]))

(deftest gen-bombs-test
  (is (= (count (core/gen-bombs 4 4 3)) 16)) ;; 4x4 grid -> 16 squares
  (is (= (reduce + (core/gen-bombs 4 4 3)) 5))) ;; 1 bomb per 3 squares, 4x4 should have 5 bombs

(deftest num-adjacent-bombs-test
  (is (= (core/num-adjacent-bombs 0 [0 0 0 0] 2 2) 0))
  (is (= (core/num-adjacent-bombs 0 [0 1 1 1] 2 2) 3)))

(deftest gen-board-test
  (is (= (-> (core/gen-board 2 2 1) (nth 1) :is-bomb) 1))
  (is (= (-> (core/gen-board 2 2 1) (nth 1) :num-adjacent-bombs) 3)))
