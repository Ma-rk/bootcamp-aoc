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




(comment
  (-> "src/input/aoc2018_7_input.txt"
      input-txt->line-vector
      refine-instructions
      get-completed-instructions))



;; part 2
;; With 5 workers and the 60+ second step durations described above,
;; how long will it take to complete all of the steps?


(defn add-first-step
  [instructions self-set]
  (let [from-letters (set (map :from instructions))
        to-letters (set (map :to instructions))
        last-letter (set/difference from-letters to-letters)]
    (conj self-set {:self (first last-letter) :prior #{}})))

(defn refined->self
  [refined-instructions]
  (let [self-set  (->> refined-instructions
                       (map (fn [x] {:self (:to x) :prior #{}}))
                       set)]
    (add-first-step refined-instructions self-set)))

(defn append-prior-step
  [self-set prior]
  (if (= (:self self-set) (:to prior))
    {:self (:self self-set) :prior (conj (:prior self-set) (:from prior))}
    self-set))

(defn put-prior-step
  [refined-instructions self-set]
  (loop [instructions refined-instructions
         self self-set]
    (if (empty? instructions)
      self
      (recur (rest instructions)
             (map (fn [x] (append-prior-step x (first instructions))) self)))))

(defn append-sec
  [self-set-with-prior]
  (map (fn [x] {:self (:self x)
                :prior (:prior x)
                :sec (- (int (first (:self x))) 4)})
       self-set-with-prior))

(defn remove-next-step-in-prior-list
  [next-steps step-set]
  (let [prior (filter (fn [prior] (not-any? (fn [next-step] (= next-step prior)) next-steps))
                      (:prior step-set))]
    {:self (:self step-set)
     :prior prior
     :sec (:sec step-set)}))

(defn tik
  [[ordered-setps rest-step-sets]]
  (let [this-step-sets (filter  #(= 0 (count (:prior %))) rest-step-sets) ; 이번에 실행되어야 할 step-set들만 골라냄
        this-steps (sort (map #(:self %) this-step-sets))  ; this-step-sets에서 이번에 실행되어야 할 step(char)만 골라냄
        new-steps (concat ordered-setps this-steps)
        next-step-sets (filter #(not= 0 (count (:prior %))) rest-step-sets)]; rest-step-sets에서 이번에 실행되어야 할 step-set들을 제거하고 나머지만 남김 
    [new-steps (map (fn [x] (remove-next-step-in-prior-list new-steps x)) next-step-sets)])); 실행된 prior를 prior 리스트에서 제거]))

(defn take-while+
  "take-while 함수가 마지막 엘리먼트를 반환하지 않는 문제를 해결하는 함수"
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(defn keep-going?
  [[_ next-step-sets]]
  (not= 0 (count next-step-sets)))

(comment
  (let [refined-instructions (-> "src/input/aoc2018_7_input.txt"
                                 input-txt->line-vector
                                 refine-instructions)
        self-set (refined->self refined-instructions)
        self-set-with-prior (put-prior-step refined-instructions self-set)
        completed-self-set (append-sec self-set-with-prior)]
    completed-self-set)



  (let [refined-instructions (-> "src/input/aoc2018_7_input.txt"
                                 input-txt->line-vector
                                 refine-instructions)
        self-set (refined->self refined-instructions)
        self-set-with-prior (put-prior-step refined-instructions self-set)
        completed-self-set (append-sec self-set-with-prior)]
    (take-while+ keep-going? (iterate tik [[] completed-self-set])))

  (+ 1 2))
