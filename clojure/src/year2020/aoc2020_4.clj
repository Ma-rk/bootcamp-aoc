(ns aoc2020_4
  (:require [clojure.string :as str]
            [common :refer [input-txt->line]]
            [clojure.spec.alpha :as s]))

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
  (->> key-value-list
       (map (fn [key-value] (let [[key value] (str/split key-value #":")] [(keyword key) value])))
       (into {})))


(defn input-line->passport-maps
  "
   input 데이터 전체를 변환하여 passport map의 coll을 반환

   input: \"eyr:2033\r\nhgt:177cm pid:173cm\r\necl:utc byr:2029 hcl:#efcc98 iyr:2023\r\n\r\npid:337605855...\"
   output: '( {\"ecl\" \"hzl\", \"hcl\" \"#a97842\", \"hgt\" \"157cm\", \"eyr\" \"2025\", \"iyr\" \"2015\", \"byr\" \"1978\", \"pid\" \"579525362\"} {\"ecl\" \"hzl\", \"hcl\" \"#a97842\" ...} ... )
   "
  [passport-line]
  (let [processing-line (-> passport-line
                            (str/replace #" " "\r\n")
                            (str/split #"\r\n\r\n"))]
    (->> processing-line
         (map (fn [line] (str/split line #"\r\n")))
         (map (fn [line] (gen-passport-map line))))))

(defn passport-has-valid-field?
  "
   필수 필드를 모두 갖고 있으면 true
   하나라도 없으면 false

   input: [{\"ecl\" \"hzl\", \"hcl\" \"#a97842\", \"hgt\" \"157cm\", \"eyr\" \"2025\", \"iyr\" \"2015\", \"byr\" \"1978\", \"pid\" \"579525362\"} ... ]
   output: 필수 필드를 모두 갖고 있는 map으로 구성된 컬렉션
   "
  [passport]
  (clojure.set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} (set (keys passport))))

(comment
  (->> "src/year2020/aoc2020_4.txt"
       input-txt->line
       input-line->passport-maps
       (filter passport-has-valid-field?)
       count))


;; part 2
;; count the passports where all required fields are both present and valid

(s/def ::byr-valid? #(<= 1920 % 2002)) ;; Birth Year. four digits; at least 1920 and at most 2002.
(s/def ::iyr-valid? #(<= 2010 % 2020)) ;; Issue Year. four digits; at least 2010 and at most 2020.
(s/def ::eyr-valid? #(<= 2020 % 2030)) ;; Expiration Year. four digits; at least 2020 and at most 2030.
(s/def ::hgt-valid? #(or (and (= (:measure %) "cm") (<= 150 (:value %) 193)) ;; Height. a number followed by either cm or in: If cm, the number must be at least 150 and at most 193.
                         (and (= (:measure %) "in") (<= 59 (:value %) 76)))) ;; If in, the number must be at least 59 and at most 76.
(s/def ::hcl-valid? #(re-matches #"#[0-9a-f]{6}" %)) ;; Hair Color. a # followed by exactly six characters 0-9 or a-f.
(s/def ::ecl-valid? #(re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" %)) ;; Eye Color. exactly one of: amb blu brn gry grn hzl oth
(s/def ::pid-valid? #(re-matches #"\d{9}" %)) ;; Passport ID. a nine-digit number, including leading zeroes.




(defn fields-have-valid-value?
  "passport-map의 모든 필드의 값이 올바른지 확인"
  [passport-map]
  (and (s/valid? ::byr-valid? (Integer/parseInt (:byr passport-map)))
       (s/valid? ::iyr-valid? (Integer/parseInt (:iyr passport-map)))
       (s/valid? ::eyr-valid? (Integer/parseInt (:eyr passport-map)))
       (s/valid? ::hgt-valid? (:hgt passport-map))
       (s/valid? ::hcl-valid? (:hcl passport-map))
       (s/valid? ::ecl-valid? (:ecl passport-map))
       (s/valid? ::pid-valid? (:pid passport-map))))

(defn append-hgt-map
  [passport]
  "
   hgt 필드를 단위와 값으로 파싱하여 map으로 변환

   input ex)
   {:ecl amb, :hgt 70, ...}
   or
   {:eyr 2033 :hgt 177cm ...}

   output ex:
   {:ecl \"brn\" :cid \"323\" :hgt nil ... }
   or
   {:pid \"192524664 :hgt {:measure \"cm\", :value 182} ... }
   "
  (let [hgt (:hgt passport)]
    (if (or (str/includes? hgt "cm")
            (str/includes? hgt "in"))
      (let [measure (subs hgt (- (count hgt) 2) (- (count hgt) 0))
            value (subs hgt 0 (- (count hgt) 2))]
        (assoc passport :hgt {:measure measure :value (Integer/parseInt value)}))
      (assoc passport :hgt nil))))

(comment
  (fields-have-valid-value? {:byr "1925",
                             :iyr "2020",
                             :eyr "2030",
                             :hgt {:measure "in", :value 70},
                             :hcl "#18171d",
                             :ecl "hzl",
                             :pid "204206116"})

  (->> "src/year2020/aoc2020_4.txt"
       input-txt->line
       input-line->passport-maps
       (filter passport-has-valid-field?)
       (map append-hgt-map)
       (filter fields-have-valid-value?)
       count))
