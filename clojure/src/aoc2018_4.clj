(ns aoc2018_4
  (:require [clojure.string :as str])
  (:require [common :refer [input-txt->line-vector]]))

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

(defn trans
  "
   문제의 input 입력받아 경비근무 하루의 기록을 한줄로 모음

   output: '(\"#2399 13 36\" \"#3373 06 19 46 51 56 58\" ... )
   "
  [lines]
  (let [merged (->> lines
                    sort
                    (reduce str))
        replaced (-> merged
                     (str/replace #"\[1518-\d{2}-\d{2} \d{2}:\d{2}\] Guard " "\r\n")
                     (str/replace #"begins shift" "")
                     (str/replace #"falls asleep" "")
                     (str/replace #"wakes up" "")
                     (str/replace #"1518-\d{2}-\d{2} 00:" "")
                     (str/replace #"\[|\]" ""))]
    (map str/trim (rest (str/split-lines replaced)))))

(defn merge-records-of-a-guard
  "
   같은 가드의 근무기록끼리 하나로 모음

   input: '(\"#2399 13 36\" \"#3373 06 19 46 51 56 58\" ... )
   
   output: {\"#2939\" {:id \"#2939\", :mins (0 41 30 59 1)},
            \"#1993\" {:id \"#1993\", :mins (22 42 14 43 ...)} ... }
   "
  [transformed-lines]
  (loop [transformed-lines-loop transformed-lines
         transformed-map {}]
    (if (empty? transformed-lines-loop)
      transformed-map
      (let [id (first (str/split (first transformed-lines-loop) #" "))
            new-mins (rest (str/split (first transformed-lines-loop) #" "))]
        (recur (rest transformed-lines-loop)
               (let [data-of-guard (get transformed-map id {:id id :mins []})
                     mins (get data-of-guard :mins)
                     concat-mins (concat mins (map #(Integer/parseInt %) new-mins))]
                 (assoc transformed-map id  {:id id :mins concat-mins})))))))

(defn get-sleep-minutes-range
  "
   졸기 시작한 시각과 일어난 시각을 계산하여 존 시간을 구함

   input: '(06 19 46 51 56 58)
   
   output: 20
   "
  [data-map]
  (let [to-range (partition 2 (:mins data-map))
        ranges (map (fn [pair] (- (last pair) (first pair))) to-range)]
    (reduce + ranges)))


(defn transform-id-sum-pair
  "
   {guard id - (존 시간)} 의 구조로 데이터를 변경

   input: {\"#2939\" {:id \"#2939\", :mins (0 41 30 59 1)},
           \"#1993\" {:id \"#1993\", :mins (22 42 14 43 ...)} ... }
   
   output: {\"#2939\" 20,
            \"#1993\" 54 ... }
   "
  [merged-record]
  (loop [k (keys merged-record)
         new-map {}]
    (if (empty? k)
      new-map
      (recur (rest k)
             (assoc new-map (first k) (get-sleep-minutes-range (get merged-record (first k))))))))

(defn get-longest-slept-guard-id
  "
   가장 오래 졸았던 가드의 id를 반환

   input: {\"#2939\" 20,
           \"#1993\" 54 ... }
   
   output: \"#1993\"
   "
  [id-sum-pair]
  (key (apply max-key val id-sum-pair)))

(defn get-most-frequantly-slept-min
  "
   가장 빈번하게 졸고 있었던 분을 리턴

   input: (22 42 14 43 ...)
   output: 10
   "
  [min]
  (let [ranged-mins (map (fn [pair] (range (first pair) (last pair))) (partition 2 min))
        concated-mins (reduce concat ranged-mins)
        freq (frequencies concated-mins)
        the-max (key (apply max-key val freq))]
    the-max))

(comment
  (let [merged-records (-> "src/input/aoc2018_4_input.txt"
                           input-txt->line-vector
                           trans
                           merge-records-of-a-guard)
        longest-slept-guard-id (-> merged-records
                                   transform-id-sum-pair
                                   get-longest-slept-guard-id)
        longest-slept-guard-mins (:mins (get merged-records longest-slept-guard-id))
        most-frequantly-slept-min (get-most-frequantly-slept-min longest-slept-guard-mins)
        int-id (Integer/parseInt (str/replace longest-slept-guard-id #"#" ""))]
    (* most-frequantly-slept-min int-id)))


;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
(defn trans-to-sleep-frequancy
  "
   input을 {(가드의 ID) (각 가드가 졸았던 횟수)} 구조를 가진 맵의 시퀀스로 변환하여 리턴
   
   input: {\"#2939\" {:id \"#2939\", :mins (0 41 30 59 1)},
            \"#1993\" {:id \"#1993\", :mins (22 42 14 43 ...)} ... }
   
   output: '({\"#1\" 41} {\"#2\" 33} ... {\"#1992\" 50} )
   "
  [input]
  (map (fn [input-vals] {(:id input-vals) (count (:mins input-vals))}) (vals input)))


(defn get-most-frequently-slept-guard-id
  "
   input 시퀀스에 포함된 맵 중에서 value(각 가드가 졸았던 횟수)가 가장 큰 맵을 찾아 가드의 id를 반환
   
   input: {\"#2939\" {:id \"#2939\", :mins (0 41 30 59 1)},
            \"#1993\" {:id \"#1993\", :mins (22 42 14 43 ...)} ... }
   
   output: '({\"#1\" 41} {\"#2\" 33} ... {\"#1992\" 50} )
   "
  [input]
  (first (keys (apply max-key #(val (first %)) input))))

(comment
  (let [merged-records (-> "src/input/aoc2018_4_input.txt"
                           input-txt->line-vector
                           trans
                           merge-records-of-a-guard)
        most-frequently-slept-guard-id (-> merged-records
                                           trans-to-sleep-frequancy
                                           get-most-frequently-slept-guard-id)
        most-frequently-slept-guard-mins (:mins (get merged-records most-frequently-slept-guard-id))
        most-frequantly-slept-min (get-most-frequantly-slept-min most-frequently-slept-guard-mins)
        int-id (Integer/parseInt (str/replace most-frequently-slept-guard-id #"#" ""))]
    (* most-frequantly-slept-min int-id)))
