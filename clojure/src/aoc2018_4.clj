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

(defn trans
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
  [data-map]
  (let [to-range (partition 2 (:mins data-map))
        ranges (map (fn [pair] (- (last pair) (first pair))) to-range)]
    (reduce + ranges)))

(defn transform-id-sum-pair
  [merged-record]
  (loop [k (keys merged-record)
         new-map {}]
    (if (empty? k)
      new-map
      (recur (rest k)
             (assoc new-map (first k) (get-sleep-minutes-range (get merged-record (first k))))))))

(defn get-max-key
  [id-sum-pair]
  (println id-sum-pair)
  (let [max-value-key  (key (apply max-key val id-sum-pair))
        max-value (get id-sum-pair max-value-key)
        key-int (Integer/parseInt (str/replace max-value-key #"#" ""))]
    (println (str "max-value: " max-value " key-int: " key-int))
    (* max-value key-int)))

(defn get-longest-slept-guard-id
  [id-sum-pair]
  (key (apply max-key val id-sum-pair)))

(defn get-most-frequantly-slept-min
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
