(ns aoc2018_5)

(defn input-txt->line-vector
  "
   입력받은 경로의 파일을 읽어 파일의 내용을 반환(vector)
   input sample: src/input/aoc2018_5_input.txt
   "
  [file-name]
  (-> file-name
      slurp))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(defn destruct-same-type-char
  "
   같은 종류의 인접한 대소문자가 없을때까지 인접한 같은 종류의 문자를 없애고 남은 문자열을 리턴

   input: dabAcCaCBAcCcaDA
   output: dabCBAcaDA
   "
  [input]
  (loop  [head [(first input)]
          tail (rest input)]
    (cond (empty? tail)
          ;; descruction 을 마치고 남은 문자열을 리턴
          head

          (= 0 (count head))
          ;; head가 빈 벡터일 때 (last head) 하면 nil이 나오는데, nil로 사칙연산을 하지 않기 위한 방어 로직
          (recur (conj head (first tail))
                 (rest tail))

          ;; 같은 타입의 문자를 desruction
          (= 32 (Math/abs  (- (last head) (first tail))))
          (recur (vec (drop-last head)) ;; (drop-last head)를 하면 vector가 아니라 LazySeq가 리턴되기 때문에 vector로 직접 변환
                 (rest tail))

          :else
          ;; tail의 첫 문자열을 head의 마지막 문자열로 옮김
          (recur (conj head (first tail))
                 (rest tail)))))


(comment
  (->> "src/input/aoc2018_5_input.txt"
       input-txt->line-vector
       (map int)
       destruct-same-type-char
       (count)))



;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn destruct-and-remove
  "
   같은 종류의 인접한 대소문자가 없을때까지 인접한 같은 종류의 문자를 없애고 남은 문자열을 리턴
   위 작업을 진행하다가 제거대상 문자열이 나오면 제거

   (clojure.string/replace  input-str #\"(?i)[a!]\" \"\") 를 이용해 보고 싶었으나
   변수를 정규표현식 안의 문자에 적용하는 방법을 찾지 못함...

   input:
   - input-str
   -- ex) dabAcCaCBAcCcaDA
   - upper
   -- ex) D
   output: destruction 완료 후 남은 문자의 개수
   - ex) 4
   "
  [input-str upper]
  (let [diff 32
        lower (+ upper diff)
        int-input (map int input-str)]
    (loop  [head [(first int-input)]
            tail (rest int-input)]
      (cond (empty? tail)
            ;; descruction 을 마치고 남은 문자열의 길이를 리턴
            (count head)

            (or (= lower (first head))
                (= upper (first head)))
            ;; 루프 진입점에서 head [(first int-input)] 로 가져온 문자열이 제거대상 문자였는지 체크
            (recur (vec (drop-last head))
                   tail)

            (= 0 (count head))
            ;; head가 빈 벡터일 때 (last head) 하면 nil이 나오는데, nil로 사칙연산을 하지 않기 위한 방어 로직
            (recur (conj head (first tail))
                   (rest tail))

            (or (= lower (first tail))
                (= upper (first tail)))
            ;; 현재 제거 대상인 문자를 제거
            (recur head
                   (rest tail))

            (= diff (Math/abs  (- (last head) (first tail))))
            ;; 같은 타입의 문자를 desruction
            (recur (vec (drop-last head))
                   (rest tail))

            :else
            ;; tail의 첫 문자열을 head의 마지막 문자열로 옮김
            (recur (conj head (first tail))
                   (rest tail))))))

(defn loop-search
  "
   A~Z에 대하여 

   input: dabAcCaCBAcCcaDA
   output: dabCBAcaDA
   "
  [input]
  (let [A 65
        Z 90]
    (loop [upper A
           result-map {}]
      (if (> upper Z)
        result-map
        (recur (inc upper)
               (assoc result-map (str (char upper)) (destruct-and-remove input upper)))))))

(->> "src/input/aoc2018_5_input.txt"
     input-txt->line-vector
     loop-search
     vals
     (apply min))
