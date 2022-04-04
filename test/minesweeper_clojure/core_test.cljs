(ns minesweeper-clojure.core-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [minesweeper-clojure.core :as core]))

(deftest gen-bombs-test
  (is (= (count (core/gen-bombs 4 4 3)) 16)) ;; 4x4 grid -> 16 squares
  (is (= (reduce + (core/gen-bombs 4 4 3)) 5))) ;; 1 bomb per 3 squares, 4x4 should have 5 bombs

(deftest edges-test
  (is (= (core/edges 0 2 2)) {:is-left-edge true, :is-right-edge false, :is-top-edge true, :is-bottom-edge false})
  (is (= (core/edges 5 4 4)) {:is-left-edge false, :is-right-edge false, :is-top-edge false, :is-bottom-edge false}))

(deftest num-adjacent-bombs-test
  (is (= (core/num-adjacent-bombs 0 [0 0 0 0] 2 2) 0))
  (is (= (core/num-adjacent-bombs 0 [0 1 1 1] 2 2) 3)))

(deftest num-adjacents-test
  (is (= (core/adjacents 0 [0 0 0 0] 2 2) [nil nil nil nil 0 nil 0 0]))
  (is (= (core/adjacents 0 [0 1 1 1] 2 2) [nil nil nil nil 1 nil 1 1])))

(deftest gen-board-test
  (is (= (-> (core/gen-board 2 2 1) (nth 1) :is-bomb) 1))
  (is (= (-> (core/gen-board 2 2 1) (nth 1) :num-adjacent-bombs) 3)))
