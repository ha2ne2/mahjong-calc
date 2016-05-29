(ns ha2ne2.mj-analyze
  (:require
   [clojure.xml :as xml]
   [clojure.zip :as z]
   [ha2ne2.util :refer :all]
   [ha2ne2.mahjong-calc :refer :all]
   [clj-http.client :as client]
   [clojure.java.io :as jio]))

(use 'clojure.test)

(defn ton-nan-filter [col]
  (filter (partial re-find #"四鳳南") col))

(defn get-ids [col]
  (mapv (fn [line]
          (let [split-data (clojure.string/split line #" \| ")
                id (re-find #"log=(.*)\"" (get split-data 3))]
            (get id 1)))
        col))

(def base-url "http://tenhou.net/0/log/?")
(def base-url "http://e.mjv.jp/0/log/index.cgi?")
;; (def raw-data (slurp (jio/resource "sample.html")))
;; (def data-list (ton-nan-filter (clojure.string/split raw-data #"\r\n")))
;; (def ids (get-ids data-list))
;; (def id (first ids))

(def file-names 
  (-> (slurp (jio/resource "gz2/file_names.txt"))
      (clojure.string/split #"\r\n")))

(defn get-ids-from-file [file-name]
  (-> (slurp (jio/resource (str "gz2/" file-name)))
      (clojure.string/split #"\r\n")
      ton-nan-filter
      get-ids))

(defn sec-to-show [n]
  (clojure.pprint/cl-format nil "~2'0d:~2'0d:~2'0d"
                            (int (/ n 3600))
                            (int (mod (/ n 60) 60))
                            (int (mod (mod n 3600) 60))))

;; (mapc-with-time
;;  (fn [x]
;;    (Thread/sleep 3000)
;;    (println x))
;;  (range 10) 1)

(defn mapc-with-time
  ([f coll]
   (mapc-with-time f coll 100))
  ([f coll interval]
   (let [start (System/currentTimeMillis)
         len (count coll)]
     (loop [[head & tail :as col] coll
            i 1
            prev start
            hist []]
       (when-not (empty? col)
         (f head)
         (let [current (System/currentTimeMillis)
               current-elapse (- current prev)
               hist (conj hist current-elapse)
               hist (vec (if (> (count hist) 50) (rest hist) hist))
               per  (/ (reduce + hist) (count hist) 1000)
               elapsed (int (/ (- current start) 1000))
               remain (int (* per (- len i)))
               total  (int (* per len))
               ]
                                        ;(println hist)
           (when (zero? (mod i interval))
             (println "   Per   Elapse   Remain    Total Count" )
             (clojure.pprint/cl-format true "~6,2f ~{~6d ~}~d/~d~%"
                                       per (map sec-to-show (list elapsed remain total)) i len))
           (recur tail (inc i) current hist)))))))


(defn extract-date [id]
  (->> id (re-find #"20(1\d\d\d)") second))

(def monthly-files
  (group-by extract-date file-names))

(defn get-path-from-id [id]
  (str (->> id (re-find #"20(1\d\d\d)") second) "/"))

(defn download-xml' [id]
  (let [file-name (str "dev-resources/" (extract-date id) "/" id ".xml")]
    (when-not (.exists (clojure.java.io/as-file file-name))
      (println id)
      (try
        (spit file-name (:body (client/get (str base-url id))))
        (catch Exception e
          (print e)
          (Thread/sleep 5000)
          (download-xml' id))))))

(defn xml-parse [s]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes s))))

(defn pai-convertor [n]
  (let [n (quot n 4)
        color-table '[m p s z]
        color (color-table (quot n 9))
        num (inc (mod n 9))]
    (if (= color 'z)
      (list ('[東 南 西 北 白 発 中] (dec num)))
      (list color num))))

;; (map (fn [[k v]] (list k (to-str (tenho->ha2ne2 v))))
;;      {:hai3 "87,35,50,48,77,63,17,123,16,44,78,133,99",
;;       :hai2 "33,61,30,72,43,58,92,128,7,88,112,129,115",
;;       :hai1 "46,45,31,93,107,20,102,108,57,117,6,69,124",
;;       :hai0 "119,34,39,0,106,105,100,38,5,49,135,120,41",})
;; ((:hai3 "4s9m44p2s7p5m北5m3p2s中7s")
;;  (:hai2 "9m7p8m1s26p6s発2m5s南発南")
;;  (:hai1 "33p8m69s6m8s東6p西2m9p白")
;;  (:hai0 "西9m1p1m998s1p2m4p中北2p"))
(defn tenho->ha2ne2 [s]
  (->> (clojure.string/split s #",")
       (map read-string)
       (map pai-convertor)))

(def tenho->ha2ne2'
  (comp ha2ne2.mahjong-calc/to-str tenho->ha2ne2))

(defn fill16 [bv]
  (vec (concat bv (repeat (- 16 (count bv)) 0))))

;; (to-binary-vec 14442)
;; [0 1 0 1 0 1 1 0 0 0 0 1 1 1]
(defn to-binary-vec [n]
  (loop [n n
         acc []]
    (if (<= n 0) (fill16 (vec acc))
        (recur (quot n 2) (conj acc (mod n 2))))))

;; (binary-vec-to-int (to-binary-vec 14442))
;; 14442
(defn binary-vec-to-int [bv]
  (loop [i 0
         base 1
         acc 0]
    (if (< i (count bv))
      (recur (inc i) (* 2 base) (+ acc (* (bv i) base)))
      acc)))
(clojure.test/is (= 64 (binary-vec-to-int [0 0 0 0 0 0 1])))

(def nakis
  '(50281 18775 48169 46090 53759 5739 26959
    48747 18432 55575 21504 45130 14442 6219
    48745 25719 25951))

(defn pon-pai-convertor [n]
  (let [color-table '[m p s z]
        color (color-table (quot n 9))
        num (inc (mod n 9))]
    (if (= color 'z)
      (list ('[東 南 西 北 白 発 中] (dec num)))
      (list color num))))

;; 1m～9mを0～8、1p～9pを9～17、1sから9sを18～26、字牌を27～33で表現する。 
(defn analyze-pon [bv]
  (let [type7 (binary-vec-to-int (subvec bv 9))
        pon-pai (quot type7 3)
        naki (mod type7 3)
        pon-pai' (pon-pai-convertor pon-pai)]
    (with-meta (repeat 3 pon-pai')
      {:kind :pon})))

; (18775 53759 26959 55575 25719 25951)

(defn analyze-ti- [bv]
  (let [type6 (binary-vec-to-int (subvec bv 10))
        min-pai (quot type6 3)
        color   (quot min-pai 7)
        num     (mod  min-pai 7)
        ti-pais (map
                 #(pon-pai-convertor (+ % (* 9 color)))
                 (range num (+ num 3)))]
    (with-meta ti-pais
      {:kind :ti-})))

(defn analyze-kakan [bv]
  (let [type7 (binary-vec-to-int (subvec bv 9))
        pon-pai (pon-pai-convertor (quot type7 3))]
    (with-meta (repeat 4 pon-pai)
      {:kind :kan})))

;; (analyze-naki 25642)
;; ((発) (発) (発) (発))

;; (18432 21504)
(defn analyze-kan [bv]
  (let [type8 (binary-vec-to-int (subvec bv 8))
        kan-pais (repeat 4 (pon-pai-convertor (quot type8 4)))
        from-who (binary-vec-to-int (subvec bv 0 2))]
    (with-meta kan-pais
      {:kind (if (= from-who 0) :ankan :kan)}
      )))

(defn analyze-naki [n]
  (let [bv (to-binary-vec n)
        f (cond
            (= 1 (bv 2)) analyze-ti-
            (= 1 (bv 3)) analyze-pon
            (= 1 (bv 4)) analyze-kakan
            :else        analyze-kan)]
    (f bv)))

(def yaku
  '[門前清自摸和 立直 一発 槍槓 嶺上開花
    海底摸月 河底撈魚 平和 断幺九 一盃口
    自風東 自風南 自風西 自風北
    場風東 場風南 場風西 場風北
    役牌白 役牌發 役牌中
    ;; 二飜
    両立直 七対子 混全帯幺九 一気通貫 三色同順
    三色同刻 三槓子 対々和 三暗刻 小三元 混老頭
    ;; 三飜
    二盃口 純全帯幺九 混一色
    ;; 六飜
    清一色
    ;; 満貫
    人和
    ;; 役満
    天和 地和 大三元 四暗刻 四暗刻単騎 字一色
    緑一色 清老頭 九蓮宝燈 純正九蓮宝燈 国士無双
    国士無双１３面 大四喜 小四喜 四槓子
    ;; 懸賞役
    ドラ 裏ドラ 赤ドラ])

;; (convert-yaku 2 1)
;; (1 一発)
(defn convert-yaku [[n han]]
  (list han (yaku n)))


(defn extract-naki-from-id% [id]
  (->> (str id ".xml")
       slurp
       (re-seq #"m=\"(\d*)\"")
       (map second)
       (map read-string)))

(defn println' [x] (do (println x) x))
(defn extract-naki-from-id [id]
  (list id (map #(list % (analyze-naki %)) (extract-naki-from-id% id))))

(defn next-pai' [[color n :as p]]
  (cond (nil? n) (let [jihai '[(東) (南) (西) (北)]
                       jihai2  '[(白) (発) (中)] 
                       zn (.indexOf jihai p)
                       zn2 (.indexOf jihai2 p)]
                   (cond (not= zn -1) (if (= zn 3) (jihai 0) (jihai (inc zn)))
                         :else        (if (= zn2 2) (jihai2 0) (jihai2 (inc zn2)))))
        :else (if (= n 9) (list color 1) (list color (inc n)))))

(testing "next-pai' test"
  (is (= (take 10 (iterate next-pai' '(東)))
         '((東) (南) (西) (北) (東) (南) (西) (北) (東) (南))))
  (is (= (take 10 (iterate next-pai' '(m 5)))
         '((m 5) (m 6) (m 7) (m 8) (m 9) (m 1) (m 2) (m 3) (m 4) (m 5))))
  (is (= (take 10 (iterate next-pai' '(白)))
         '((白) (発) (中) (白) (発) (中) (白) (発) (中) (白)))))
  


;; (take 3 ids)
;; ("2016011622gm-00a9-0000-ca0e77fe" "2016011622gm-00a9-0000-9a033bc6" "2016011622gm-00a9-0000-6341426f")
;; (time (flatten (take 3 (map get-agari ids))))
(defn convert-agari [{:keys [hai doraHai doraHaiUra ten yaku yakuman m who fromWho machi] :as agari-data}
                     {:keys [ba kyoku oya]
                      :or {ba '東 kyoku 1 oya 0} :as ba-data}]
  (let [[hu ten] (map read-string (clojure.string/split ten #","))
        machi (->> machi read-string pai-convertor)
        naki (when m (map (comp analyze-naki read-string)
                          (clojure.string/split m #",")))
        yaku (if yaku
               (->> (clojure.string/split yaku #",")
                  (map read-string)
                  (partition 2)
                  (map convert-yaku))
               (->> (clojure.string/split yakuman #",")
                  (map read-string)
                  (map #(vector % 13))
                  (map convert-yaku)))
        aux (filter (comp '#{天和 地和 両立直 立直 一発 嶺上開花 海底摸月 河底撈魚 槍槓 赤ドラ} second)
                    yaku)
        who (read-string who)
        fromWho (read-string fromWho)
        {:keys [pon ti- kan ankan]} (group-by (comp :kind meta) naki)]
    (array-map :ba ba
               :kyoku kyoku
               :ie ('[東 南 西 北] (mod (- who oya) 4))
               :oya (= oya who)
               :menzen (remove-x machi (tenho->ha2ne2 hai))
               :naki naki
               :pon (seq pon)
               :ti- (seq ti-)
               :kan (seq kan)
               :ankan ankan
               :dora (map (comp next-pai' pai-convertor read-string)
                          (clojure.string/split doraHai #","))
               :ura-dora (when doraHaiUra
                           (map (comp next-pai' pai-convertor read-string)
                                (clojure.string/split doraHaiUra #",")))
               :tumo (when (= who fromWho) machi)
               :ron  (when-not (= who fromWho) machi)
               :yaku' yaku
               :hu' hu
               :ten' ten
               :aux aux
               :orig agari-data
               )))

(defn conv [direction n]
  {:ba ('[東 南 西 北] direction)
   :kyoku (inc n)
   :oya n})

;; (ba-conv '(0 1 2 2 3 0 0 1 2 3 0 1))
;; ("東1局" "東2局" "東3局" "東3局" "東4局"
;;  "南1局" "南1局" "南2局" "南3局" "南4局"
;;  "西1局" "西2局")
(defn ba-conv [lst]
  (loop [[a b & r :as lst] lst
         direction 0
         result nil]
    (if (nil? b)
      (reverse (conj result (conv direction a)))
      (recur (rest lst)
             (mod (+ direction (if (= [3 0] [a b]) 1 0)) 4)
             (conj result (conv direction a))))))

;; (split-by zero? '(0 1 2 0 1 6 7 0 3 2 4))
;; ((0 1 2) (0 1 6 7) (0 3 2 4))
(defn split-by [f lst]
  (map seq
   (loop [[h & t :as lst] lst
          acc []
          result []]
     (cond (empty? lst) (conj result acc)
           (f h) (recur t (conj [] h)
                        (if (empty? acc) result
                            (conj result acc)))
           :else (recur t (conj acc h) result)))))


(defn add-ba-info [[ba-info & agaris]]
  (map #(convert-agari % ba-info) agaris))

(defn get-agari [id]
  (letfn [(parse [id]
            (->> (str "dev-resources/" id ".xml")
                 slurp
                 xml-parse
                 xml-seq
                 (filter #(some (=c (:tag %)) '(:INIT :AGARI)))
                 (map #(case (:tag %)
                         :INIT (read-string (:oya (:attrs %)))
                         :AGARI (:attrs %)))
                 (split-by number?)))
          (join [data]
            (mapcat add-ba-info
                    (map #(cons % %2)
                         (ba-conv (map first data))
                         (map rest data))))]
    (join (parse id))))

(defn get-agari' [path id]
  (get-agari (str path id)))

;; (convert-agari (first raw-agari-data) '{:ba 東 :kyoku 0 :oya 0})

;; {:ba 東, :kyoku 0, :ie 北, :oya false, :menzen ((m 4) (m 4) (m 5) (m 5) (m 5) (p 2) (p 3) (p 4) (s 2) (s 2) (s 2) (s 3) (s 4) (s 5)), :naki nil, :pon nil, :ti- nil, :kan nil, :ankan nil, :dora ((p 5)), :ura-dora nil, :yaku' ((1 門前清自摸和) (1 断幺九) (1 赤ドラ)), :hu' 30, :ten' 4000, :aux ((1 門前清自摸和))}

(def raw-agari-data '({:who "3", :hai "13,15,16,17,18,40,44,48,77,78,79,83,87,91", :fromWho "3", :ba "0,0", :doraHai "54", :yaku "0,1,8,1,54,1", :machi "18", :ten "30,4000,0", :sc "250,-20,250,-10,250,-10,250,40"} {:who "3", :hai "1,2,5,6,8,10,36,39,77,80,83,84,87,88", :fromWho "1", :ba "0,0", :doraHai "81", :yaku "7,1,9,1,52,2,54,1", :machi "1", :ten "30,8000,1", :sc "230,0,240,-80,240,0,290,80"} {:who "2", :hai "6,8,15,18,21,24,41,43,59,61,65,79,81,84", :fromWho "0", :ba "0,0", :doraHai "54", :yaku "7,1,8,1,52,1", :machi "59", :ten "30,5800,0", :sc "230,-58,160,0,240,58,370,0"} {:who "0", :hai "25,30,34,46,47,52,58,62,75,76,80,81,85,91", :fromWho "0", :ba "1,1", :doraHai "103", :doraHaiUra "7", :yaku "1,1,0,1,7,1,54,1,53,0", :machi "80", :ten "20,5200,0", :sc "162,65,160,-14,298,-27,370,-14"} nil nil {:who "2", :hai "54,57,61,84,89,92,122,123", :fromWho "3", :ba "2,1", :doraHai "111", :m "14442,45130", :yaku "12,1", :machi "84", :ten "30,1000,0", :sc "277,0,126,0,251,26,336,-16"} {:who "2", :hai "8,9,10,24,29,34,58,60,67,84,87,92,96,103", :fromWho "3", :ba "0,1", :doraHai "69", :doraHaiUra "20", :yaku "1,1,53,1", :machi "67", :ten "40,2600,0", :sc "277,0,126,0,267,36,320,-26"} {:who "0", :hai "17,21,26,33,34,83,86,89,93,94,95,128,130,131", :fromWho "0", :ba "0,0", :doraHai "76", :yaku "0,1,19,1,52,1", :machi "93", :ten "40,5200,0", :sc "277,52,126,-13,303,-26,294,-13"} {:who "0", :hai "5,7,9,13,16,21,26,30,45,51,53,56,63,66", :fromWho "1", :ba "0,0", :doraHai "90", :owari "368,47.0,74,-43.0,277,-12.0,281,8.0", :yaku "7,1,8,1,54,1", :machi "53", :ten "30,3900,0", :sc "329,39,113,-39,277,0,281,0"}))


;; (def agari-samples (get-agari id))
(def agari-sample
  {:who "2",
   :hai "54,57,61,84,89,92,122,123",
   :fromWho "3",
   :ba "2,1",
   :doraHai "111",
   :m "14442,45130",
   :yaku "12,1",
   :machi "84",
   :ten "30,1000,0",
   :sc "277,0,126,0,251,26,336,-16"})

(clojure.test/is
 (= (split-by
     (=c :INIT)
     '(:INIT :AGARI
       :INIT :AGARI :AGARI
       :INIT :RYUUKYOKU
       :INIT :AGARI))
    '((:INIT :AGARI)
      (:INIT :AGARI :AGARI)
      (:INIT :RYUUKYOKU)
      (:INIT :AGARI))))


  ;; 34種類 136枚
(clojure.test/testing "pai-convertor"
  (clojure.test/is (= '(m 9) (pai-convertor 35)))
  (clojure.test/is (= '(p 2) (pai-convertor 43)))
  (clojure.test/is (= '(s 5) (pai-convertor 88)))
  (clojure.test/is (= '(中) (pai-convertor 135))))


(clojure.test/testing "pon-pai-convertor"
  (clojure.test/is (= '(p 1) (pon-pai-convertor 9)))
  (clojure.test/is (= '(西) (pon-pai-convertor 29)))
  (clojure.test/is (= (map pon-pai-convertor (range 34))
                      '((m 1) (m 2) (m 3) (m 4) (m 5) (m 6) (m 7) (m 8) (m 9)
                        (p 1) (p 2) (p 3) (p 4) (p 5) (p 6) (p 7) (p 8) (p 9)
                        (s 1) (s 2) (s 3) (s 4) (s 5) (s 6) (s 7) (s 8) (s 9)
                        (東) (南) (西) (北) (白) (発) (中)))))

(clojure.test/testing "analyze-pon"
  (clojure.test/is (= (analyze-naki 14442) '((p 1) (p 1) (p 1))))
  (clojure.test/is (= (analyze-naki 45130) '((西) (西) (西)))))

(clojure.test/testing "ti-"
  (clojure.test/is (= (analyze-naki 18775) '((m 7) (m 8) (m 9))))
  (clojure.test/is (= (analyze-naki 53759) '((s 4) (s 5) (s 6))))
  (clojure.test/is (= (analyze-naki 26959) '((p 2) (p 3) (p 4)))))

(def agari-sample 
  '{:ba 東, :kyoku 1, :ie 北, :oya false,
    :menzen ((m 4) (m 4) (m 5) (m 5) (m 5) (p 2) (p 3) (p 4) (s 2) (s 2) (s 2) (s 3) (s 4) (s 5)),
    :naki nil, :pon nil, :ti- nil, :kan nil, :ankan nil,
    :dora ((p 5)), :ura-dora nil, :yaku "0,1,8,1,54,1", :hu' 30, :ten' 4000})

(defn extract-by [m keys]
  (map #(% m) keys))

(defn extract-by' [m & keys]
  (apply concat-array-map (map #(array-map % (% m)) keys)))

(defn calc-by-myself [lst]
  (extract-by (first (nan-ten?% lst)) [:符 :点]))

(defn calc-by-tenho [lst]
  (extract-by lst [:hu' :ten']))

(defn tenho-test [agari-data]
  (let [s (figure-to-str (first (figure-out agari-data)))
        [me tenho] ((juxt calc-by-myself calc-by-tenho) agari-data)]
    (if (= me tenho)
      (do ;;(println "PASS:" s me)
          true)
      (do (println " FAIL:" s me tenho agari-data) false))))

(defn agari-100-test []
  (reduce (fn [acc agari-data]
            (update
             (if (tenho-test agari-data)
               (update acc :pass inc)
               (update acc :fail inc))
             :test inc))
          {:test 0, :pass 0, :fail 0}
          agari-data100))

(defn agari-test [agari-data-list]
  (reduce
   (fn [acc result]
     (when (zero? (mod (acc :test) 500))
       (println acc))
      (update
       (if result
         (update acc :pass inc)
         (update acc :fail inc))
       :test inc))
   {:test 0, :pass 0, :fail 0}
   (pmap tenho-test agari-data-list)))


;;(def agari-data100 (take 100 (mapcat get-agari ids)))
(defn monthly-test [month-str]
  (let [files  (monthly-files month-str)
        ids    (mapcat get-ids-from-file files)
        agaris (mapcat #(get-agari' (str month-str "/") %) ids)]
    (agari-test agaris)))

(defn year-test []
  (let [ids    (mapcat get-ids-from-file file-names)
        agaris (mapcat #(get-agari' (get-path-from-id %) %) ids)]
    (agari-test agaris)))
