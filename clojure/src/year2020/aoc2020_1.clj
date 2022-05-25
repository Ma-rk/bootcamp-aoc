(ns aoc2020_1
  (:require [clojure.string :as string]))

(defn input-txt->line-vector
  "
   입력받은 경로의 파일을 읽어 파일의 내용을 반환(vector)

   input sample: src/year2020/aoc2020_1.txt
   output sample: [\"408\" \"1614\" \"1321\" \"1028\" \"1018\" \"2008\" \"1061\" ... ]
   "
  [file-name]
  (->
   file-name
   slurp
   string/split-lines))

;; part 1
;; find the two entries that sum to 2020 and then multiply those two numbers together.

;; For example, suppose your expense report contained the following:
;; 1721
;; 979
;; 366
;; 299
;; 675
;; 1456
;; In this list, the two entries that sum to 2020 are 1721 and 299.
;; Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

(defn last-loop
  [head-num rest-seq]
  (loop [rest-seq-loop rest-seq]
    (let [num-2 (first rest-seq-loop)]
      (cond
        (empty? rest-seq-loop)
        nil

        (= 2020 (+ head-num num-2))
        (* head-num num-2)

        :else
        (recur (rest rest-seq-loop))))))

(defn first-loop
  [input-vector]
  (loop [num-1 (first input-vector)
         rest-seq (rest input-vector)]
    (if-let [num-1-resutl (last-loop num-1 rest-seq)]
      num-1-resutl
      (recur (first rest-seq) (rest rest-seq)))))

(->> "src/year2020/aoc2020_1.txt"
     input-txt->line-vector
     (map #(Integer/parseInt %))
     first-loop)

;; part 2
;; They offer you a second one if you can find three numbers in your expense report
;; that meet the same criteria.

;; Using the above example again, the three entries that sum to 2020 are 979, 366, and 675.
;; Multiplying them together produces the answer, 241861950.

;; In your expense report, what is the product of the three entries that sum to 2020?

(defn in-loop
  [out-num mid-num rest-seq]
  (loop [rest-seq-loop rest-seq]
    (let [in-num (first rest-seq-loop)]
      (cond
        (empty? rest-seq-loop)
        nil

        (= 2020 (+ out-num mid-num in-num))
        (* out-num mid-num in-num)

        :else
        (recur (rest rest-seq-loop))))))

(defn mid-loop
  [out-num input-vector]
  (println (format "%s" out-num))

  (loop [mid-num (first input-vector)
         rest-seq (rest input-vector)]
    (let [num-1-resutl (in-loop out-num mid-num rest-seq)]
      (cond (empty? rest-seq)
            nil

            num-1-resutl
            num-1-resutl

            :else
            (recur (first rest-seq) (rest rest-seq))))))

(defn out-loop
  [input-vector]
  (loop [num-1 (first input-vector)
         rest-seq (rest input-vector)]
    (let [num-1-resutl (mid-loop num-1 rest-seq)]
      (cond num-1-resutl
            num-1-resutl

            :else
            (recur (first rest-seq) (rest rest-seq))))))

(->> "src/year2020/aoc2020_1.txt"
     input-txt->line-vector
     (map #(Integer/parseInt %))
     out-loop)