(ns minesweeper-clojure.core-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [minesweeper-clojure.core :as core]))

(deftest gen-mines-test
  (is (= (count (core/gen-mines 4 4 3)) 16)) ;; 4x4 grid -> 16 squares
  (is (= (reduce + (core/gen-mines 4 4 3)) 5))) ;; 1 mine per 3 squares, 4x4 should have 5 mines

(deftest edges-test
  (is (= (core/edges 0 2 2)) {:is-left-edge true, :is-right-edge false, :is-top-edge true, :is-bottom-edge false})
  (is (= (core/edges 5 4 4)) {:is-left-edge false, :is-right-edge false, :is-top-edge false, :is-bottom-edge false}))

(deftest adjacent-indices-test
  (is (= (core/adjacent-indices 0 2 2) '(1 2 3)))
  (is (= (core/adjacent-indices 13 10 10) '(12 14 3 23 2 4 22 24))))

(deftest num-adjacent-mines-test
  (is (= (core/num-adjacent-mines 0 [0 0 0 0] 2 2) 0))
  (is (= (core/num-adjacent-mines 0 [0 1 1 1] 2 2) 3)))

(deftest gen-board-test
  (is (= (-> (core/gen-board 2 2 1) (nth 1) :is-mine) 1))
  (is (= (-> (core/gen-board 2 2 1) (nth 1) :num-adjacent-mines) 3)))
