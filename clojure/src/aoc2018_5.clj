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


(defn find-reaction-index
  "
   같은 종류의 소문자와 대문자가 인접해 있는 위치를 찾아
   인접한 두 문자 중 앞 문자의 index를 반환

   input: \"dabAcCaCBAcCcaDA\"
   
   output: 4
   "
  [input-str]
  (let [input-loop (map int input-str)]
    (loop [current-index 0]
      (cond (= (+ 1 current-index) (count input-loop)) -1
            (= 32 (Math/abs (- (nth input-loop current-index) (nth input-loop (+ 1 current-index))))) current-index
            :else (recur (inc current-index))))))

(defn trigger-destruction
  "
   입력받은 위치와 그 다음 위치의 문자열을 지우고 남은 문자열을 반환

   input
   - input-str
   -- ex) \"dabAcCaCBAcCcaDA\"
   - reaction-index --
   -- ex) 
   
   output: 인접한 같은 종류의 두 문자를 지우고 남은 문자열
   - ex) \"dabAaCBAcCcaDA\"
  "
  [input-str reaction-index]
  (str (subs input-str 0 reaction-index) (subs input-str (+ 2 reaction-index))))

(defn get-reducted-str
  "
   인접한 같은 종류의 소문자와 대문자를 지우고 남은 문자열에서 같은 작업을 더이상 지울 문자가 없을때까지 반복한다

   input: \"dabAcCaCBAcCcaDA\"
   
   output: \"dabCBAcaDA\"
   "
  [original-str]
  (loop [str-loop original-str]
    (println (count str-loop))
    (println str-loop)
    (let [reaction-index (find-reaction-index str-loop)]
      (if (= -1 reaction-index)
        str-loop
        (recur (trigger-destruction str-loop reaction-index))))))

(comment
  (-> "src/input/aoc2018_5_input.txt"
      input-txt->line-vector
      get-reducted-str))


;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.
