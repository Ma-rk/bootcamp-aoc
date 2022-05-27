(ns aoc2018_6
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
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
  (let [splitted-lines (map (fn [line] (str/split line #", ")) inputs)]
    (map (fn [[x y]] {:point-x (Integer/parseInt x) :point-y (Integer/parseInt y)}) splitted-lines)))

(defn get-board-size
  [point-list]
  (let [max-x (apply max (map :point-x point-list))
        max-y (apply max (map :point-y point-list))]
    {:max-x max-x :max-y max-y}))

(defn get-grids
  [grid-size-pair]
  (for [y (range (inc (:max-y grid-size-pair)))
        x (range (inc (:max-x grid-size-pair)))] {:grid-x x :grid-y y}))


(defn get-dist
  [grid point]
  (let [x-diff (Math/abs (- (:grid-x grid) (:point-x point)))
        y-diff (Math/abs (- (:grid-y grid) (:point-y point)))]
    (+ x-diff y-diff)))

(defn get-nearest-point
  [grid point-list]
  (let [dist-point-list (sort-by :dist (map (fn [point] {:dist (get-dist grid point) :point point}) point-list))
        dist-1 (:dist (nth dist-point-list 0))
        dist-2 (:dist (nth dist-point-list 1))]
    (cond
      (= dist-1 0)
      :point

      (= dist-1 dist-2)
      :tied

      :else
      (:point (first dist-point-list)))))

(defn mark-nearest-point-on-grid
  [points grids]
  (map (fn [grid] (assoc grid :nearest-point (get-nearest-point grid points))) grids))

(defn reached-to-edge?
  [marked-grid board-size]
  (or (= (:grid-x marked-grid) 0)
      (= (:grid-x marked-grid) (:max-x board-size))
      (= (:grid-y marked-grid) 0)
      (= (:grid-y marked-grid) (:max-y board-size))))

(defn remove-infinite-point
  [marked-grids board-size]
  (let [reached-grids (filter (fn [grid] (reached-to-edge? grid board-size)) marked-grids)
        infinite-points (distinct (map :nearest-point reached-grids))]
    (remove-infinite-point3 marked-grids infinite-points)))

;; 제대로 동작하지 않음: 108 라인 let 블록의 테스트 데이터에서는 제대로 동작하는데
;; 119 라인의 let 블록을 통해 실행했을때는 어떤 grid도 remove되지 않는다.
;; some의 동작이 LazySeq에 대해서는 다르게 동작하는건가...
(defn remove-infinite-point3
  [marked-grids infinite-points]
  (remove (fn [marked-grid] (some #(= (:nearest-poin marked-grid) %) (vector infinite-points))) marked-grids))

(let [aa  [{:grid-x 6, :grid-y 9, :nearest-poin {:point-x 1, :point-y 1}}
           {:grid-x 6, :grid-y 9, :nearest-poin {:point-x 8, :point-y 3}}
           {:grid-x 6, :grid-y 9, :nearest-poin {:point-x 8, :point-y 2}}]
      bb '({:point-x 1, :point-y 1}
           :tied
           {:point-x 8, :point-y 3}
           :point
           {:point-x 1, :point-y 6}
           {:point-x 8, :point-y 9})]
  (remove-infinite-point3 aa bb))

(let [points  (-> "src/input/aoc2018_6_input.txt"
                  input-txt->line-vector
                  convert-input-to-map)
      board-size (-> points
                     get-board-size)
      grids (-> board-size
                get-grids)
      marked-grids (mark-nearest-point-on-grid points grids)]
  (remove-infinite-point marked-grids board-size))

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
