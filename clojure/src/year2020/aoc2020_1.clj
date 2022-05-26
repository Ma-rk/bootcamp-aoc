(ns aoc2020_1
  (:require [clojure.string :as string])
  (:require [common :refer [input-txt->line-int-vector]]))


;; part 1
;; find the two entries that sum to 2020 and then multiply those two numbers together.

;; For example, suppose your expense report contained the following:
;; 1721
;; 979
;; 366
;; 299
;; 675
;; 1456
;; In this list, the two entries that sum to 2020 are 1721 and 299.
;; Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.


(def int-list [1 2 3])

(for [x int-list
      y int-list
      :when (= 5 (+ x y))]
  [x y])

(defn find-and-multiply-two-entries
  [int-list]
  (let [matrix-list (for [x int-list y int-list] [x y])
        pair-2020-list (filter (fn [[x y]] (= 2020 (+ x y))) matrix-list)
        pair-2020 (first pair-2020-list)]
    (reduce * pair-2020)))

(comment
  (-> "src/year2020/aoc2020_1.txt"
      input-txt->line-int-vector
      find-and-multiply-two-entries))

;; part 2
;; They offer you a second one if you can find three numbers in your expense report
;; that meet the same criteria.

;; Using the above example again, the three entries that sum to 2020 are 979, 366, and 675.
;; Multiplying them together produces the answer, 241861950.

;; In your expense report, what is the product of the three entries that sum to 2020?

(defn find-three-entries
  [int-list]
  (for [x int-list
        y int-list
        z int-list
        :when (= 2020 (+ x y z))]
    [x y z]))

(defn get-multiply
  [found-list]
  (reduce * (first found-list)))

(comment
  (->> "src/year2020/aoc2020_1.txt"
       input-txt->line-int-vector
       find-three-entries
       get-multiply))
