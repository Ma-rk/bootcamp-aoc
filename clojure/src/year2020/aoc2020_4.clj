(ns aoc2020_4
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:require [common :refer [input-txt->line]]))


;; part 1
;; Count the number of valid passports - those that have all required fields. Treat cid as optional.
;; In your batch file, how many passports are valid?
(defn gen-passport-map
  "
   input 데이터 중 여권 하나에 해당하는 정보를 입력받아 key-value의 map 구조로 변환하여 리턴

   input: [hcl:0cacc5 byr:1944 iyr:2028 eyr:2024 hgt:163in pid:74169361 ecl:dne]
   output:   {\"hcl\" \"0cacc5\", \"byr\" \"1944\", \"iyr\" \"2028\", \"eyr\" \"2024\", \"hgt\" \"163in\", \"pid\" \"74169361\", \"ecl\" \"dne\"}
   "
  [key-value-list]
  (loop [key-value-loop key-value-list
         passport-map {}]
    (if (empty? key-value-loop) passport-map
        (let [key (first (str/split (first key-value-loop) #":"))
              val (last (str/split (first key-value-loop) #":"))]
          (recur (rest key-value-loop)
                 (assoc passport-map key val))))))

(defn convert-passport-line-to-passport-map
  "
   input 데이터 전체를 변환하여 passport map의 coll을 반환

   input: \"eyr:2033\r\nhgt:177cm pid:173cm\r\necl:utc byr:2029 hcl:#efcc98 iyr:2023\r\n\r\npid:337605855...\"
   output: '( {\"ecl\" \"hzl\", \"hcl\" \"#a97842\", \"hgt\" \"157cm\", \"eyr\" \"2025\", \"iyr\" \"2015\", \"byr\" \"1978\", \"pid\" \"579525362\"} {\"ecl\" \"hzl\", \"hcl\" \"#a97842\" ...} ... )
   "
  [passport-lines]
  (let [a (-> passport-lines
              (str/replace #" " "\r\n")
              (str/split #"\r\n\r\n"))]
    (->> a
         (map (fn [line] (str/split line #"\r\n")))
         (map (fn [line] (gen-passport-map line))))))

(defn filter-valid-passport-via-field
  "
   필수 필드를 모두 갖고 있으면 true
   하나라도 없으면 false

   input: {\"ecl\" \"hzl\", \"hcl\" \"#a97842\", \"hgt\" \"157cm\", \"eyr\" \"2025\", \"iyr\" \"2015\", \"byr\" \"1978\", \"pid\" \"579525362\"}
   output: false
   "
  [passports]
  (filter (fn [passport] (and (contains? passport "byr")
                              (contains? passport "iyr")
                              (contains? passport "eyr")
                              (contains? passport "hgt")
                              (contains? passport "hcl")
                              (contains? passport "ecl")
                              (contains? passport "pid"))) passports))

(comment
  (-> "src/year2020/aoc2020_4.txt"
      input-txt->line
      convert-passport-line-to-passport-map
      filter-valid-passport-via-field
      count))



;; part 2
;; count the passports where all required fields are both present and valid
