(ns minesweeper-clojure.core-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [minesweeper-clojure.core :as core]))

(deftest gen-mines-test
  (is (= (core/gen-mines 2 2 0) [false false false false]))
  (is (= (core/gen-mines 2 2 4) [true true true true])))

(deftest edges-test
  (is (= (core/edges 0 2 2)) {:is-left-edge true, :is-right-edge false, :is-top-edge true, :is-bottom-edge false})
  (is (= (core/edges 5 4 4)) {:is-left-edge false, :is-right-edge false, :is-top-edge false, :is-bottom-edge false}))

(deftest adjacent-indices-test
  (is (= (core/adjacent-indices 0 2 2) '(1 2 3)))
  (is (= (core/adjacent-indices 13 10 10) '(12 14 3 23 2 4 22 24))))

(deftest bool->int
  (is (= (map core/bool->int [false false true false]) '(0 0 1 0))))

(deftest num-adjacent-mines-test
  (is (zero? (core/num-adjacent-mines 0 [false false false false] 2 2)))
  (is (= (core/num-adjacent-mines 0 [false true true true] 2 2) 3)))

(deftest gen-board-test
  (is (true? (-> (core/gen-board 2 2 4) (nth 1) :is-mine)))
  (is (= (-> (core/gen-board 2 2 4) (nth 1) :num-adjacent-mines) 3)))
