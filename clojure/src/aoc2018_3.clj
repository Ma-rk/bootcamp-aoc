(ns aoc2018_3
  (:require [clojure.set :as set]
            [clojure.string :as str]))


(defn get-day3-input-as-list
  "
   입력받은 경로의 파일을 읽어 파일의 내용을 반환

   input sample: src/input/aoc2018_3_input.txt
   output sample: [\"#1 @ 1,3: 4x4\", ... \"#3 @ 5,5: 2x2\"]
   "
  [file-name]
  (->
   file-name
   slurp
   str/split-lines))

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)


(defn get-points-of-upper-line
  "
   사각형의 상단 라인을 구성하는 점의 번호들을 리턴
   
   n: 전체 febric의 가로 폭
   left: 상단 라인의 왼쪽 시작점
   top: 상단 라인과 febric의 최상부의 거리
   width: 상단 라인의 가로 폭

   output: #{3 4 5 6}
   "
  [n
   left
   top
   width]
  (let [left-top-angle (+ (* (- top 1) n) left)
        right-top-angle (+ left-top-angle (- width 1))]
    (set (range left-top-angle
                (+ right-top-angle 1)))))


(defn get-points-of-box
  "
   사각형의 상단 라인을 구성하는 점의 번호들을 입력받아 사각형 전체를 구헝하는 모든 점의 번호를 리턴
   
   top-line-points: 사각형의 상단 라인을 구성하는 점의 번호들
   n: 전체 febric의 가로 폭
   hight: 사각형의 높이
   
   output: (14 11 12 13 21 18 19 20 28 25 26 27)
   "
  [top-line-points
   n
   hight]
  (loop [result-list '()
         h (- hight 1)]
    (if (< h 0)
      result-list
      (recur  (into result-list (map #(+ (* n h) %) top-line-points))
              (- h 1)))))

(defn line->box
  "
   문제의 input의 한 line을 입력받아 사각형의 4개의 점의 번호를 가진 set을 리턴

   line: #1 @ 1,3: 4x4
   
   output:{:left 1, :top 3, :width 4, :hight 4}
   "
  [line]
  (let [splitted (str/split (clojure.string/replace line ":" "") #" ")
        left-top-point (nth splitted 2)
        left (Integer/parseInt (first (str/split left-top-point #",")))
        top (Integer/parseInt (last (str/split left-top-point #",")))
        area (nth splitted 3)
        width (Integer/parseInt (first (str/split area #"x")))
        hight (Integer/parseInt (last (str/split area #"x")))]
    {:left left :top top :width width :hight hight}))

(defn line-list->box-list
  "
   문제의 input의 전체를 입력받아 사각형의 4개의 점의 번호를 가진 set의 리스트로 변환하여 리턴
   "
  [line-list]
  (map line->box line-list))

(defn get-biggest-width
  "
   input 전부를 분석해서 전체 febric의 가로 폭을 계산(모든 라인의 우측 끝점 중 가장 큰 수를 리턴)

   line: #1 @ 1,3: 4x4
   
   output: 435
   "
  [given-box-list]
  (loop [loop-box-list given-box-list
         biggest-width 0]
    (if (empty? loop-box-list)
      biggest-width
      (let [left (:left (first loop-box-list))
            width (:width (first loop-box-list))
            most-right-point (- (+ left width)  1)
            biggest-width (max biggest-width most-right-point)]
        (recur (rest loop-box-list)
               biggest-width)))))

(defn execute
  [every-box-list]
  (let [biggest-width (get-biggest-width every-box-list)]
    (loop [every-box every-box-list
           inter-sector #{}
           whole-sector #{}]
      (if (empty? every-box)
        [inter-sector whole-sector]
        (let [box (first every-box)
              left (:left box)
              top (:top box)
              width (:width box)
              hight (:hight box)
              top-line-points (get-points-of-upper-line biggest-width left top width)
              points-of-box (set (get-points-of-box top-line-points biggest-width hight))]
          (recur (rest every-box)
                 (set/union inter-sector (set/intersection points-of-box whole-sector))
                 (set/union whole-sector points-of-box)))))))

(comment
  (-> "src/input/aoc2018_3_input.txt"
      (get-day3-input-as-list)
      (line-list->box-list)
      (execute)
      (first)
      (count))

  (->
   (get-points-of-upper-line 7 4 2 4))

  (->
   (get-points-of-upper-line 7 4 2 4)
   (get-points-of-box 7 3)
   (set))

  (->
   (line->box "#1 @ 287,428: 27x20"))
  (-> "src/input/aoc2018_3_input.txt"
      (get-day3-input-as-list)
      (line-list->box-list)))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
