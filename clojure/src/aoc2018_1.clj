(ns aoc2018-1
  (:require [common :refer [input-txt->line-int-vector]]))
;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(comment
  (->> "src/input/aoc2018_1_input.txt"
       input-txt->line-int-vector
       (reduce +)))


(defn get-sum
  "주어진 입력의 모든 숫자를 더함"
  [input]
  (loop [in input
         temp-sum 0]
    (if (empty? in)
      temp-sum
      (recur (rest in)
             (+ temp-sum (first in))))))

(comment
  (-> "src/input/aoc2018_1_input.txt"
      input-txt->line-int-vector
      get-sum))

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
(defn find-second
  "주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 
   처음으로 두번 나오는 숫자를 리턴"
  [input]
  (loop [input-loop input
         sum-of-frequency 0
         set-of-sum #{}]
    (if (contains? set-of-sum sum-of-frequency)
      sum-of-frequency
      (recur (if (empty? (rest input-loop))
               input
               (rest input-loop))
             (+ sum-of-frequency (first input-loop))
             (conj set-of-sum sum-of-frequency)))))

(comment
  (-> "src/input/aoc2018_1_input.txt"
      input-txt->line-int-vector
      find-second))

