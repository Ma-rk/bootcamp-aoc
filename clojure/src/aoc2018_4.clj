(ns aoc2018_4
  (:require [clojure.string :as str]))

(defn input-txt->line-vector
  "
   입력받은 경로의 파일을 읽어 파일의 내용을 반환(vector)
   input sample: src/input/aoc2018_4_input.txt
   output sample: [\"#1 @ 1,3: 4x4\", ... \"#3 @ 5,5: 2x2\"]
   "
  [file-name]
  (->
   file-name
   slurp
   str/split-lines))

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
(defn append-separator
  "
   문제의 input의 line을 입력받아 경비근무 시작의 기록일 경우 문자열 aaa를 붙임
   
   input:
   - line: 문제의 input의 line
   -- example value: [1518-06-17 23:58] Guard #3373 begins shift
   
   output:
   - (anonymous): 경비근무 시작의 기록일 경우 line의 앞에 문자열 aaa를 붙임. 그렇지 않은 경우 line 그대로 리턴
   -- example value: aaa[1518-06-17 23:58] Guard #3373 begins shift
   "
  [line]
  (if (str/includes? line "Guard")
    (str "aaa" line)
    line))

(defn merge-records-of-one-day
  "
   문제의 input 입력받아 경비근무 하루의 기록을 한줄로 모음

   input:
   - line: 문제의 input text 파일 전체
   -- example value: 
   
   output:
   - (anonymous): 한줄로 모아진 경비근무 하루의 기록
   -- example value: [1518-03-22 00:03] Guard #2137 begins shift[1518-03-22 00:21] falls asleep[1518-03-22 00:38] wakes up[1518-03-22 00:51] falls asleep[1518-03-22 00:53] wakes up
   "
  [input-lines]
  (let [sorted-input-lines (sort input-lines)
        added-input-lines (map append-separator sorted-input-lines)
        merged-input-lines (reduce str added-input-lines)
        replaced-line (str/replace merged-input-lines #"aaa" "\r\n")
        splitted-line (str/split-lines replaced-line)]
    splitted-line))

(comment
  (-> "src/input/aoc2018_4_input.txt"
      input-txt->line-vector
      sort
      merge-records-of-one-day))

(defn line->no
  "
   문제의 input의 한 line을 입력받아 guard의 번호를 추출
   input:
   - line: 문제의 input text 파일의 한 line
   -- example value: [1518-11-08 00:02] Guard #2851 begins shift
   
   output:
   - (anonymous): guard의 번호
   -- example value: 2851
   "
  [line]
  (-> line
      (str/split #"#")
      last
      (str/split #" ")
      first
      Integer/parseInt))

(comment
  (-> "[1518-11-08 00:02] Guard #2851 begins shift"
      line->no))

(defn line->mins
  "
   경비의 하루 근무 기록을 나타내는 line을 입력받아 졸기 시작한 분과 깨어난 분만 추출

   input:
   - line: 경비의 하루 근무 기록을 나타내는 line
   -- example: [1518-03-18 00:00] Guard #3491 begins shift[1518-03-18 00:15] falls asleep[1518-03-18 00:27] wakes up[1518-03-18 00:46] falls asleep[1518-03-18 00:49] wakes up[1518-03-18 00:54] falls asleep[1518-03-18 00:59] wakes up

   output: 졸기 시작한 분과 깨어난 분만 추출한 seq
   "
  [line]
  (let [str1 (str/replace line #"begins shift" "")
        str2 (str/replace str1 #"falls asleep" "")
        str3 (str/replace str2 #"wakes up" "")
        str4 (str/replace str3 #"1518-\d{2}-\d{2} 00:" "")
        str5 (str/replace str4 #"\[|\]" "")
        str6 (rest (str/split str5 #"#"))
        str7 (first str6)
        str8 (str/split str7 #" ")
        str9 (rest str8)]
    str9))

(comment
  (->  "[1518-03-18 00:00] Guard #3491 begins shift[1518-03-18 00:15] falls asleep[1518-03-18 00:27] wakes up[1518-03-18 00:46] falls asleep[1518-03-18 00:49] wakes up[1518-03-18 00:54] falls asleep[1518-03-18 00:59] wakes up"
       line->mins))



(defn lines->records
  [lines]
  (loop [lines-loop lines
         records {}]
    (let [guard-no (line->no (first lines-loop))
          mins ()]
      (cond
        (empty? lines-loop)                        records
        (str/includes? (first lines-loop) "Guard") (recur (rest lines-loop)
                                                          (assoc records (line->no (first lines-loop)) nil))
        :else (recur (rest lines-loop)
                     records)))))

(comment
  (->
   "src/input/aoc2018_4_input.txt"
   input-txt->line-vector
   sort
   lines->records))

(comment
  (->
   "src/input/aoc2018_4_input.txt"
   input-txt->line-vector
   sort))



;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
