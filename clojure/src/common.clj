(ns common
  (:require [clojure.string :as str]))

(defn input-txt->line
  "
   입력받은 경로의 파일을 읽어 파일의 내용을 반환(단일 line)
   input sample: src/input.txt
   "
  [file-name]
  (-> file-name
      slurp))

(defn input-txt->line-vector
  "
   입력받은 경로의 파일을 읽어 파일의 내용을 반환(vector)
   input sample: src/input.txt
   "
  [file-name]
  (-> file-name
      input-txt->line
      str/split-lines))

(defn input-txt->line-int-vector
  "
   입력받은 경로의 파일을 읽어 파일의 내용을 반환(vector)
   input sample: src/input.txt
   "
  [file-name]
  (->> file-name
       input-txt->line
       str/split-lines
       (map #(Integer/parseInt %))))
