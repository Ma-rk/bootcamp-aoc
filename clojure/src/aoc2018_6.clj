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
  "입력파일을 읽어 point 정보를 가진 map으로 변환

  output ex) '({:pt-x 78, :pt-y 335} {:pt-x 74, :pt-y 309} {:pt-x 277, :pt-y 44} ...)"
  [lines]
  (let [splitted-lines (map #(str/split % #", ") lines)]
    (map (fn [[x y]] {:pt-x (Integer/parseInt x) :pt-y (Integer/parseInt y)}) splitted-lines)))

(defn points->board-size
  "points를 분석해 전체 좌표계의 사이즈를 측정

  input ex) '({:pt-x 78, :pt-y 335} {:pt-x 74, :pt-y 309} {:pt-x 277, :pt-y 44} ...)
  output ex) {:max-x 353, :max-y 357}"
  [points]
  (let [max-x (apply max (map :pt-x points))
        max-y (apply max (map :pt-y points))]
    {:max-x max-x :max-y max-y}))

(defn board-size->grids
  "좌표계의 사이즈를 이용해 전체 좌표계를 나타내는 map으로 구성된 collection을 생성

  input ex) {:max-x 353, :max-y 357}
  output ex) '({:gd-x 0, :gd-y 0} {:gd-x 1, :gd-y 0} {:gd-x 2, :gd-y 0} {:gd-x 3, :gd-y 0} {:gd-x 4, :gd-y 0} {:gd-x 5, :gd-y 0} {:gd-x 6, :gd-y 0} {:gd-x 7, :gd-y 0} {:gd-x 8, :gd-y 0} {:gd-x 0, :gd-y 1} {:gd-x 1, :gd-y 1} ... )"
  [board-size]
  (for [y (range (inc (board-size :max-y)))
        x (range (inc (board-size :max-x)))]
    {:gd-x x :gd-y y}))

(defn get-dist
  "두 점(하나의 grid와 하나의 point) 사이의 멘하탄 거리를 측정

  input ex)
  - grid: {:gd-x 8, :gd-y 3}
  - point {:pt-x 306, :pt-y 102}
  output ex) 397"
  [grid point]
  (let [diff-x (Math/abs (- (grid :gd-x) (point :pt-x)))
        diff-y (Math/abs (- (grid :gd-y) (point :pt-y)))]
    (+ diff-x diff-y)))

(defn get-nearest-point-from-grid
  "모든 point 중 특정 grid에서 가장 가까운 point를 찾음
  가장 가까운 point가 하나보다 많으면 :tied를 반환

  input ex)
  - grid: {:gd-x 8, :gd-y 3}
  - points: '({:pt-x 78, :pt-y 335} {:pt-x 74, :pt-y 309} {:pt-x 277, :pt-y 44} ...)  
  output ex) {:pt-x 306, :pt-y 102} or :tied"
  [grid points]
  (let [points-with-dist (map (fn [point] {:point point :dist (get-dist grid point)}) points)
        orderd-points  (sort-by :dist points-with-dist)
        dist-1 ((nth orderd-points 0) :dist)
        dist-2 ((nth orderd-points 1) :dist)]
    (if (= dist-1 dist-2)
      :tied
      (:point (first orderd-points)))))

(defn mark-nearest-point-on-grid
  "모든 grid에 대하여 각각 가장 가까운 point를 표시
  가장 가까운 point가 하나가 아니면 :tied를 표시

  input ex) '({:gd-x 0, :gd-y 0} {:gd-x 1, :gd-y 0} {:gd-x 2, :gd-y 0} {:gd-x 3, :gd-y 0} {:gd-x 4, :gd-y 0} {:gd-x 5, :gd-y 0} {:gd-x 6, :gd-y 0} {:gd-x 7, :gd-y 0} {:gd-x 8, :gd-y 0} {:gd-x 0, :gd-y 1} {:gd-x 1, :gd-y 1} ... )
  output ex) '{:gd-x 4, :gd-y 0, :nearest-pt {:pt-x 1, :pt-y 1}} {:gd-x 5, :gd-y 0, :nearest-pt :tied} {:gd-x 6, :gd-y 0, :nearest-pt {:pt-x 8, :pt-y 3}} ...)"
  [grids points]
  (map (fn [grid] (assoc grid :nearest-pt (get-nearest-point-from-grid grid points))) grids))

(defn collect-infitite-points
  "영역이 무한히 확장되는 points를 찾음
  
  input ex) '{:gd-x 4, :gd-y 0, :nearest-pt {:pt-x 1, :pt-y 1}} {:gd-x 5, :gd-y 0, :nearest-pt :tied} {:gd-x 6, :gd-y 0, :nearest-pt {:pt-x 8, :pt-y 3}} ...) 
  output ex) '({:pt-x 1, :pt-y 1} {:pt-x 8, :pt-y 3} {:pt-x 1, :pt-y 6} {:pt-x 8, :pt-y 9})"
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
  "영역이 유한한 points를 찾음

  input ex) '({:pt-x 1, :pt-y 1} {:pt-x 8, :pt-y 3} {:pt-x 1, :pt-y 6} {:pt-x 8, :pt-y 9})
  output ex) #{{:pt-x 3, :pt-y 4} {:pt-x 5, :pt-y 5}}"
  [points infitite-points]
  (st/difference (set points) (set infitite-points)))

(defn get-count-of-most-great-finite-points
  "영역이 유한한 point 중 가장 영역이 넓은 point의 넓이를 반환
  
  input ex)
  - marked-grids: '{:gd-x 4, :gd-y 0, :nearest-pt {:pt-x 1, :pt-y 1}} {:gd-x 5, :gd-y 0, :nearest-pt :tied} {:gd-x 6, :gd-y 0, :nearest-pt {:pt-x 8, :pt-y 3}} ...) 
  - finite-points: #{{:pt-x 3, :pt-y 4} {:pt-x 5, :pt-y 5}} 
  output ex) 17"
  [marked-grids finite-points]
  (->> (map :nearest-pt marked-grids)
       (filter (fn [nearest-pt] (contains? finite-points nearest-pt)))
       (frequencies)))

(comment
  (let [points (-> "src/input/aoc2018_6_input.txt"
                   input-txt->line-vector
                   lines->points)
        max-xy (points->board-size points)
        grids (board-size->grids max-xy)
        marked-grids (mark-nearest-point-on-grid grids points)
        count-of-finite-points (->> marked-grids
                                    collect-infitite-points
                                    (get-finite-point points)
                                    (get-count-of-most-great-finite-points marked-grids))]
    (val (first (sort-by val > count-of-finite-points))))
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

(defn get-dist-from-points
  "하나의 drid를 기준으로 모든 point와의 맨하탄 거리의 합을 구함

  input ex) {:gd-x 8, :gd-y 3}
  output ex) '({:pt-x 78, :pt-y 335} {:pt-x 74, :pt-y 309} {:pt-x 277, :pt-y 44} ...)"
  [grid points]
  (reduce + (map (fn [point] (get-dist grid point)) points)))

(defn mark-distance-on-grid
  "grid와 모든 point들간의 거리의 합을 구해 grid에 표시

  input ex) '({:gd-x 0, :gd-y 0} {:gd-x 1, :gd-y 0} {:gd-x 2, :gd-y 0} {:gd-x 3, :gd-y 0} {:gd-x 4, :gd-y 0} {:gd-x 5, :gd-y 0} {:gd-x 6, :gd-y 0} {:gd-x 7, :gd-y 0} {:gd-x 8, :gd-y 0} {:gd-x 0, :gd-y 1} {:gd-x 1, :gd-y 1} ... )
  output ex) "
  [grids points]
  (map (fn [grid] (assoc grid :dist (get-dist-from-points grid points)))  grids))

(defn get-count-of-dist-under-10000
  "grids 중에서 다른 point들과의 거리의 합이 10000 미만인 drid의 개수를 구함

  input ex) ({:gd-x 0, :gd-y 0, :dist 20335} {:gd-x 1, :gd-y 0, :dist 20285} {:gd-x 2, :gd-y 0, :dist 20235} {:gd-x 3, :gd-y 0, :dist 20185} ...)
  output ex) 15"
  [dist-appended-grids]
  (count (filter (fn [grid] (< (:dist grid) 10000)) dist-appended-grids)))


(comment
  (let [points (-> "src/input/aoc2018_6_input.txt"
                   input-txt->line-vector
                   lines->points)
        grids (-> points
                  points->board-size
                  board-size->grids)]
    (-> (mark-distance-on-grid grids points)
        get-count-of-dist-under-10000)))
