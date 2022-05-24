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
          head

          (= 0 (count head))
          (recur (conj head (first tail))
                 (rest tail))

          (= 32 (Math/abs  (- (last head) (first tail))))
          (recur (vec (drop-last head))
                 (rest tail))

          :else
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
