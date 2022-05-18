(ns aoc2018_3
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn input-txt->line-vector
  "
   입력받은 경로의 파일을 읽어 파일의 내용을 반환(vector)

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

(defn line->box
  "
   문제의 input의 한 line을 입력받아 box로 변환

   input:
   - line: 문제의 input text 파일의 한 line
   -- example value: \"#1388 @ 962,907: 17x16\"
   
   output:
   - (anonymous): 사각형의 속성을 나타내는 map
   -- example value: {:id 1388 :left 962, :top 907, :width 17, :hight 16}
   "
  [line]
  (let [splitted (str/split (clojure.string/replace line ":" "") #" ")
        id (clojure.string/replace (nth splitted 0) "#" "")
        left-top-pixel (nth splitted 2)
        left (Integer/parseInt (first (str/split left-top-pixel #",")))
        top (Integer/parseInt (last (str/split left-top-pixel #",")))
        area (nth splitted 3)
        width (Integer/parseInt (first (str/split area #"x")))
        hight (Integer/parseInt (last (str/split area #"x")))]
    {:id id :left left :top top :width width :hight hight}))


(defn line-vector->box-seq
  "
   input의 모든 line을 입력받아 box의 seq로 변환

   input:
   - line-vector: 문제의 input text 파일의 각 line으로 구성된 vector
   -- example value: [\"#1388 @ 962,907: 17x16\" \"#1388 @ 962,907: 17x16\" ...]
   
   output:
   - (anonymous): box로 구성된 seq
   -- example value: ({:id 1388 :left 962, :top 907, :width 17, :hight 16} ...)
   "
  [line-vector]
  (map line->box line-vector))


(defn get-febric-width
  "
   input 전부를 분석해서 전체 febric의 가로 폭을 계산(모든 라인의 우측 끝점 중 가장 큰 수를 리턴)

   input:
   - box-seq:  box로 구성된 seq
   -- example value: ({:id 1388 :left 962, :top 907, :width 17, :hight 16} ...)
   
   output:
   - fegric-width: 전체 febric의 가로 폭
   -- example value: 746
   "
  [box-seq]
  (loop [loop-box-list box-seq
         fegric-width 0]
    (if (empty? loop-box-list)
      fegric-width
      (let [left (:left (first loop-box-list))
            width (:width (first loop-box-list))
            most-right-pixel (- (+ left width)  1)
            fegric-width (max fegric-width most-right-pixel)]
        (recur (rest loop-box-list)
               fegric-width)))))

(defn box->box-pixels
  "
   전체 febric의 가로 폭과 box를 입력받아 box를 구성하는 모든 pixel의 번호를 리턴

   input:
   - febric-width: 전체 febric의 가로 폭
   -- example value: 746
   - box: box
   -- example value: {:id 1388 :left 962, :top 907, :width 17, :hight 16}
   
   output:
   - box-pixels: 하나의 box를 구성하는 pixel의 번호들
   -- example value: (4 5 6 10 11 12)
   "
  [febric-width box]
  (let [left-top-angle (+ (* (- (:top box) 1) febric-width) (:left box))
        right-top-angle (+ left-top-angle (- (:width box) 1))
        top-line-pixels (range left-top-angle (+ right-top-angle 1))
        hight (:hight box)]
    (loop [box-pixels '()
           h (- hight 1)]
      (if (< h 0)
        box-pixels
        (recur  (into box-pixels (map #(+ (* febric-width h) %) top-line-pixels))
                (- h 1))))))

(defn box-seq->box-pixels-seq
  "
   box의 seq를 입력받아 box를 구성하는 모든 pixel의 번호로 구성된 seq의 seq를 리턴

   input:
   - box-seq: box의 seq
   -- example value: ({:id 1388 :left 962, :top 907, :width 17, :hight 16} ...)
   
   output:
   - (anonymous): box를 구성하는 모든 pixel의 번호로 구성된 seq의 seq
   -- example value: ((1 2 3) (4 5 6) ...)
   "
  [box-seq]
  (let [febric-width (get-febric-width box-seq)]
    (map (fn [box] (box->box-pixels febric-width box)) box-seq)))

(defn box-pixels-seq->inter-pixel-set
  "
   모든 box의 pixel들을 입력받아 2개 이상의 box에 속한 pixel 번호를 set에 담아 리턴

   input
   - box-pixels-seq: box를 구성하는 모든 pixel의 번호로 구성된 seq의 seq
   -- example value: ((1 2 3) (4 5 6) ...)
   output: 
   - inter-pixel: 서로 다른 box간에 겹치는 pixel의 번호가 담긴 set
   -- example value: #(15 16 21 22)
   "
  [box-pixels-seq]
  (loop [whole-pixel #{}
         inter-pixel #{}
         box-pixels-loop box-pixels-seq]
    (if (empty? box-pixels-loop)
      inter-pixel
      (recur (clojure.set/union whole-pixel (set (first box-pixels-loop)))
             (clojure.set/union inter-pixel (clojure.set/intersection whole-pixel (set (first box-pixels-loop))))
             (rest box-pixels-loop)))))

(comment
  (->
   "src/input/aoc2018_3_input.txt"
   (input-txt->line-vector)
   (line-vector->box-seq)
   (box-seq->box-pixels-seq)
   (box-pixels-seq->inter-pixel-set)
   (count))

  (+ 1 1))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
