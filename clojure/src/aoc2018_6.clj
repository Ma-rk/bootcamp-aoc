(ns aoc2018_6
  (:require [clojure.string :as str]
            [clojure.set :as st]
            [clojure.pprint :as pp]
            [common :refer [input-txt->line-vector]]))

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
(defn lines->points
  [lines]
  (let [splitted-lines (map #(str/split % #", ") lines)]
    (map (fn [[x y]] {:pt-x (Integer/parseInt x) :pt-y (Integer/parseInt y)}) splitted-lines)))

(defn points->board-size
  [points]
  (let [max-x (apply max (map :pt-x points))
        max-y (apply max (map :pt-y points))]
    {:max-x max-x :max-y max-y}))

(defn board-size->grids
  [board-size]
  (for [y (range (inc (board-size :max-y)))
        x (range (inc (board-size :max-x)))]
    {:gd-x x :gd-y y}))

(defn get-dist
  [grid point]
  (let [diff-x (Math/abs (- (grid :gd-x) (point :pt-x)))
        diff-y (Math/abs (- (grid :gd-y) (point :pt-y)))]
    (+ diff-x diff-y)))

(defn get-nearest-point-from-grid
  [grid points]
  (let [points-with-dist (map (fn [point] {:point point :dist (get-dist grid point)}) points)
        orderd-points  (sort-by :dist points-with-dist)
        dist-1 ((nth orderd-points 0) :dist)
        dist-2 ((nth orderd-points 1) :dist)]
    (if (= dist-1 dist-2)
      :tied
      (:point (first orderd-points)))))

(defn mark-nearest-point-on-grid
  [grids points]
  (map (fn [grid] (assoc grid :nearest-pt (get-nearest-point-from-grid grid points))) grids))

(defn collect-infitite-points
  [marked-grids]
  (let [max-x (apply max (map :gd-x marked-grids))
        max-y (apply max (map :gd-y marked-grids))
        edge-reached-grids (filter (fn [grid] (or (= (:gd-x grid) max-x)
                                                  (= (:gd-y grid) max-y)
                                                  (= (:gd-x grid) 0)
                                                  (= (:gd-y grid) 0))) marked-grids)
        infitite-points (map :nearest-pt edge-reached-grids)
        distinct-points (distinct infitite-points)]
    (filter (fn [point] (and (not= :tied point) (not= :point point))) distinct-points)))

(defn get-finite-point
  [infitite-points points]
  (st/difference (set points) (set infitite-points)))

(defn get-count-of-finite-points
  [marked-grids finite-points]
  (->> (filter (fn [grid] (contains? finite-points (:nearest-pt grid))) marked-grids)
       (map :nearest-pt)
       (frequencies)))

(comment
  (let [points (-> "src/input/aoc2018_6_input.txt"
                   input-txt->line-vector
                   lines->points)
        max-xy (points->board-size points)
        grids (board-size->grids max-xy)
        marked-grids (mark-nearest-point-on-grid grids points)
        infitite-points (collect-infitite-points marked-grids)
        finite-points (get-finite-point infitite-points points)
        count-of-finite-points (get-count-of-finite-points marked-grids finite-points)]
    (last (first (sort-by val > count-of-finite-points))))


  (vals  [{:pt-x 54, :pt-y 75} 1587,
          {:pt-x 282, :pt-y 67} 2000,
          {:pt-x 191, :pt-y 92} 4887]))
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
