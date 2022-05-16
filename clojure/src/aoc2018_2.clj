(ns aoc2018-2)

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12
(defn get-day2-input-as-list
  "
   입력받은 경로의 파일을 읽어 파일의 내용을 반환

   input sample: src/input/aoc2018_2_input.txt
   output sample: [\"ayitmcjvlhedbsyoqfzukjpxwt\", ... \"agirmcjvl\"]
   "
  [file-name]
  (.split (slurp file-name) "\r\n"))

(defn get-occurence-of-char
  "
   문자열의 list를 입력받아
   문자열에서 반복등장하는 횟수에 해당하는 숫자들을 가진 set의 list를 반환
   n번 등장하는 문자가 하나라도 있으면 n이 포함됨

   input sample: [\"aab\", \"cdd\", \"efffgg\", ...]
   output sample: '(#{1 2} #{1 2} #{1 3 2})
   "
  [data]
  (map set (map vals (map frequencies data))))

(defn merge-sets-into-single-list
  "
   문자가 반복등장하는 횟수를 담은 set으로 구성된 list를 입력받아 모든 요소들을 하나의 list에 합쳐 반환

   input sample: '(#{1 2} #{1 2} #{1 3 2})
   output sample: (1 2 1 2 1 3 2)
   "
  [num-set-list]
  (loop [num-set num-set-list
         result-list '()]
    (if (empty? num-set)
      result-list
      (recur (rest num-set)
             (concat result-list (into '() (first num-set)))))))

(defn get-result
  "
   숫자로 구성된 list를 입력받아 list에 담긴 2의 개수와 3의 개수의 곱을 반환
   input sample: (1 2 1 2 1 3 2)
   output sample: 3"
  [occurence-cnt-list]
  (* (get (frequencies occurence-cnt-list) 2 1) (get (frequencies occurence-cnt-list) 3 1)))

(comment
  (->
   (get-day2-input-as-list "src/input/aoc2018_2_input.txt")
   (get-occurence-of-char)
   (merge-sets-into-single-list)
   (get-result)))

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################
