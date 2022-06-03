(ns aoc2018_7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [common :refer [input-txt->line-vector]]))

;; part 1
;; In what order should the steps in your instructions be completed?


(defn refine-instructions
  "output ex: '({:from \"B\", :to \"G\"} {:from \"G\", :to \"J\"}  {:from \"J\", :to \"F\"}"
  [lines]
  (->> lines
       (map (fn [line] (-> line
                           (str/replace #"Step " "")
                           (str/replace #" can begin." "")
                           (str/split #" must be finished before step "))))
       (map (fn [splited-line]  {:from (first splited-line) :to (last splited-line)}))))

(defn find-next-entry-step
  "instructions에 남은 step들 중 from에는 있지만 to에는 없는 step을 찾아
   그중 알파벳 순서가 가장 빠른 step을 반환한다.
   찾은 step을 from으로 갖고 있는 instruction들은 instructions에서 제거하고 남은 것을 반환한다.

   input ex) '({:from \"B\", :to \"G\"} {:from \"G\", :to \"J\"} {:from \"J\", :to \"F\"} {:from \"U\", :to \"Z\"} ... )
   output ex) 
   - next-entry-step: B
   - rest-instructions: '({:from \"G\", :to \"J\"} {:from \"J\", :to \"F\"} {:from \"U\", :to \"Z\"} ... )
   "
  [[_ instructions]]
  (let [form-steps (map :from instructions)
        to-steps (map :to instructions)
        next-entry-step (first (sort (into [] (set/difference (set form-steps) (set to-steps)))))
        rest-instructions (remove (fn [instruction] (= next-entry-step (:from instruction))) instructions)]
    [next-entry-step
     rest-instructions]))

(defn get-completed-instructions
  "instructions를 입력받아 올바른 조립 순서를 뜻하는 string을 만들어 반환한다.
  
   input ex) '({:from \"B\", :to \"G\"} {:from \"G\", :to \"J\"} {:from \"J\", :to \"F\"} {:from \"U\", :to \"Z\"} ... )
   output ex) CABDFE
   "
  [instructions]
  (->> ["" instructions]
       (iterate find-next-entry-step)
       (take-while #(not (nil? (first %))))
       (map #(first %))
       (reduce str)
       (append-last-letter instructions)))


(defn append-last-letter
  "get-completed-instructions 함수는 from instructions만 모아 조립순서를 만들기 때문에
   가장 마지막 instruction(to instructions에만 있는)는 누락된다.
   이 함수에서 그 instruction를 찾아 추가한다.
   
   input ex)
   - completed-instructions: CABDF
   - instructions: '({:from \"B\", :to \"G\"} {:from \"G\", :to \"J\"} {:from \"J\", :to \"F\"} {:from \"U\", :to \"Z\"} ... )
   output ex) CABDFE
   "
  [instructions completed-instructions]
  (let [completed-letters (set (map str (set completed-instructions)))
        to-letters (set (map :to instructions))
        last-letter (set/difference to-letters completed-letters)]
    (str completed-instructions (first last-letter))))


(comment
  (-> "src/input/aoc2018_7_input.txt"
      input-txt->line-vector
      refine-instructions
      get-completed-instructions))
;; part 2
;; With 5 workers and the 60+ second step durations described above,
;; how long will it take to complete all of the steps?
