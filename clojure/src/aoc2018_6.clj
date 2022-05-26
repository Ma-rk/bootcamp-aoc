(ns aoc2018_6
  (:require [clojure.string :as str])
  (:require [common :refer [input-txt->line-vector]]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(defn convert-input-to-map
  [inputs]
  (println inputs)
  (map (fn [line] (str/split line #", ")) inputs))

(-> "src/input/aoc2018_6_input.txt"
    input-txt->line-vector
    convert-input-to-map)

(defn get-board-size
  [point-list]
  (let [max-x (apply max (map first point-list))
        max-y (apply max (map (fn [y] (last y)) point-list))]
    [max-x max-y]))

(defn get-grid-list
  [board-size-pair]
  (for [y (range (inc (last board-size-pair)))
        x (range (inc (first board-size-pair)))] [x y]))

(defn get-dist
  [grid-coor point-coor]
  (let [x-diff (Math/abs (- (first grid-coor) (first point-coor)))
        y-diff (Math/abs (- (last grid-coor) (last point-coor)))]
    (+ x-diff y-diff)))

(defn get-nearest-point
  [grid point-list]
  (let [dist-point-list (sort (map (fn [point] [(get-dist grid point) point]) point-list))
        dist-1 (first (nth dist-point-list 0))
        dist-2 (first (nth dist-point-list 1))]
    (cond
      (= dist-1 0)
      :point

      (= dist-1 dist-2)
      :tied

      :else
      (last (nth dist-point-list 0)))))

(defn mark-nearest-point-on-grid
  [point-list grid-list]
  (map (fn [grid]  [grid (get-nearest-point grid point-list)]) grid-list))

(defn reached-to-edge?
  [marked-grid]
  (let [grid (first marked-grid)]
    (if (or (= 0 (first grid))
            (= 8 (first grid))
            (= 0 (last grid))
            (= 9 (last grid)))
      true
      false)))

(defn remove-infinit-point
  [area-list edge-reached-point]
  (filter (fn [[x y]] (contains? edge-reached-point x)) area-list))

(defn get-area
  [marked-grid-list]
  (let [point-only-list (map last marked-grid-list)]
    (frequencies point-only-list)))

(defn merge-point
  [filterd-marked-grid-list]
  (set (map last filterd-marked-grid-list)))

(let [point-list (sample-input)
      grid-list (-> point-list
                    get-board-size
                    get-grid-list)
      marked-grid-list (mark-nearest-point-on-grid point-list grid-list)
      area-list (get-area marked-grid-list)
      edge-reached-point (merge-point (filter reached-to-edge? marked-grid-list))
      aa (remove-infinit-point area-list edge-reached-point)]
  aa)

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.
