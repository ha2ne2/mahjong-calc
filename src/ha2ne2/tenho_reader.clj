(ns ha2ne2.tenho-reader
  (:require
   [clojure.xml :as xml]
   [clojure.zip :as z]
   [ha2ne2.util :refer :all]
   [ha2ne2.mahjong-calc :refer :all]
   [clj-http.client :as client]
   [clojure.java.io :as jio]))

(use 'clojure.test)

;; [str] -> [str]
(defn ton-nan-filter [col]
  (filter (partial re-find #"四鳳南喰赤") col))

;; [str] -> [id]
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
  (-> (slurp (jio/resource "html/index.txt"))
      (clojure.string/split #"\n")))

;; (get-ids-from-file (file-names 1))
;; ["2014010200gm-00a9-0000-98e3445a" "2014010200gm-00a9-0000-444fede6" ...
(defn get-ids-from-file [file-name]
  (-> (slurp (jio/resource (str "html/" file-name)))
      (clojure.string/split #"\r\n")
      ton-nan-filter
      get-ids))

(defn extract-date [id]
  (->> id (re-find #"20(1\d\d\d)") second))

;; {"1404" ["scc20140401.html" "scc20140402.html" ...
(def monthly-files
  (group-by extract-date file-names))

(defn get-path-from-id [id]
  (str (extract-date id) "/"))

(defn download-xml [id]
  (let [file-name (str "resources/" (extract-date id) "/" id ".xml")]
    (when-not (.exists (clojure.java.io/as-file file-name))
      (println id)
      (try
        (spit file-name (:body (client/get (str base-url id))))
        (catch Exception e
          (print e)
          (Thread/sleep 5000)
          (download-xml id))))))

(defn start-download []
  (let [ids (mapcat get-ids-from-file file-names)]
    (mapc' download-xml ids 20)))

(defn xml-parse [s]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes s))))

(defn pai-convertor [n]
  (case n
    16  (with-meta '(m 5) {:red true})
    52 (with-meta '(p 5) {:red true})
    88 (with-meta '(s 5) {:red true})
    (let [n (quot n 4)
          color-table '[m p s z]
          color (color-table (quot n 9))
          num (inc (mod n 9))]
      (if (= color 'z)
        (list ('[東 南 西 北 白 発 中] (dec num)))
        (list color num)))))

;; (map read-string (clojure.string/split s #"," hai))
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

;;(map analyze-naki '(18775 53759 26959 55575 25719 25951))
(defn analyze-ti- [bv]
  (let [type6 (binary-vec-to-int (subvec bv 10))
        offset  (mapv (comp binary-vec-to-int vec) (partition 2 (subvec bv 3 9)))
        min-pai (quot type6 3)
        ti-pai (* (+ (* 9 (quot min-pai 7)) (mod min-pai 7)) 4)
        ti-pais (map #(pai-convertor
                       (+ ti-pai (* % 4) (offset %)))
                     '(0 1 2))
        ]
    (with-meta ti-pais
      {:kind :ti-})))

;; 1m～9mを0～8、1p～9pを9～17、1sから9sを18～26、字牌を27～33で表現する。 
;; (analyze-naki 25642)
;; ((p 8) (p 8) (p 8))
(defn analyze-pon [bv]
  (let [type7 (binary-vec-to-int (subvec bv 9))
        pon-pai (* (quot type7 3) 4)
        unused (binary-vec-to-int (subvec bv 5 7))
        pon-pais (map #(pai-convertor (+ pon-pai %))
                      (remove-x unused '(0 1 2 3)))
        ]
    (with-meta pon-pais
      {:kind :pon})))

(defn analyze-kakan [bv]
  (let [type7 (binary-vec-to-int (subvec bv 9))
        kan-pai (* (quot type7 3) 4)
        kan-pais (map #(pai-convertor (+ kan-pai %))
                      '(0 1 2 3))
        ]
    (with-meta kan-pais
      {:kind :kan})))

;; (map analyze-naki '(18432 21504))
;; (((s 1) (s 1) (s 1) (s 1)) ((s 4) (s 4) (s 4) (s 4)))
(defn analyze-kan [bv]
  (let [type8 (binary-vec-to-int (subvec bv 8))
        kan-pai (* (quot type8 4) 4)
        kan-pais (map #(pai-convertor (+ kan-pai %)) '(0 1 2 3))
        from-who (binary-vec-to-int (subvec bv 0 2))]
    (with-meta kan-pais
      {:kind (if (= from-who 0) :ankan :kan)}
      )))

(defn analyze-naki [n]
  (let [bv (to-binary-vec n)
        f (cond
            (= (bv 2) 1) analyze-ti-
            (= (bv 3) 1) analyze-pon
            (= (bv 4) 1) analyze-kakan
            :else        analyze-kan)]
    (f bv)))

(def yaku
  '[門前自摸 立直 一発 槍槓 嶺上開花
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
                      :or {ba '東 kyoku 1 oya 0} :as ba-data}
                     id]
  (let [[hu ten] (map read-string (clojure.string/split ten #","))
        machi-num (read-string machi)
        machi (pai-convertor machi-num)
        menzen (map pai-convertor
                    (remove-x machi-num
                              (map read-string (clojure.string/split hai #","))))
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
        aux (empty-to-nil 
             (filter (comp '#{天和 地和 両立直 立直 一発 嶺上開花 海底摸月 河底撈魚 槍槓} second)
                     yaku))
        who (read-string who)
        fromWho (read-string fromWho)
        {:keys [pon ti- kan ankan]} (group-by (comp :kind meta) naki)]
    (array-map :ba ba
               :kyoku kyoku
               :ie ('[東 南 西 北] (mod (- who oya) 4))
               :oya (= oya who)
               :menzen menzen
               :naki naki
               :aka (map first (filter (comp :red meta) (concat menzen (list machi) (flatten1 naki))))
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
               :id id
               )))

;; (split-by zero? '(0 1 2 2 3 3 0 0 0 1 2 3 0 1))
;; ((0 1 2 2 3 3) (0) (0) (0 1 2 3) (0 1))
(defn split-by [f lst]
  (map seq
       (loop [[h & t :as l] (rest lst)
              acc [(first lst)]
              result []]
         (cond (empty? l) (conj result acc)
               (f h) (recur t [h] (conj result acc))
               :else (recur t (conj acc h) result)))))

;; (split-by' zero? '(0 1 2 2 3 3 0 0 0 1 2 3 0 1))
;; ((0 1 2 2 3 3) (0 0 0 1 2 3) (0 1))
(defn split-by' [f lst]
  (map seq
       (loop [[h & t :as l] lst
              prev true
              acc []
              acc2 []]
         (if (empty? l) (conj acc2 acc)
             (let [curr (f h)]
               (if (and curr (not prev))
                 (recur t curr [h] (conj acc2 acc))
                 (recur t curr (conj acc h) acc2)))))))


(defn conv [ba n]
  {:ba ba
   :kyoku (inc n)
   :oya n})

;; (ba-conv '(0 1 2 2 3 0 0 1 2 3 0 1))
;; ("東1局" "東2局" "東3局" "東3局" "東4局"
;;  "南1局" "南1局" "南2局" "南3局" "南4局"
;;  "西1局" "西2局")
(defn ba-conv [lst]
  (mapcat #(map (partial conv %) %2)
          (flatten (repeat '(東 南 西 北)))
          (split-by' zero? lst)))

(defn get-agari [id]
  (letfn [(parse [id]
            (->> (jio/resource (str id ".xml"))
                 slurp
                 xml-parse
                 xml-seq
                 (filter #(some (=c (:tag %)) '(:INIT :AGARI)))
                 (map #(case (:tag %)
                         :INIT (read-string (:oya (:attrs %)))
                         :AGARI (:attrs %)))
                 (split-by number?)))
          (convert [data]
            (mapcat (fn [ba-info agaris] (map #(convert-agari % ba-info id) agaris))
                    (ba-conv (map first data))
                    (map rest data)))]
    (convert (parse id))))


(defn get-agari' [path id]
  (get-agari (str path id)))

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
      (do (println "\n" (agari-data :id) "\n FAIL:" s me tenho agari-data) false))))

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

(defn agari-test2
  ([n] (agari-test2 0 n))
  ([start n]
   (let [ids (mapcat get-ids-from-file (monthly-files "1501"))
         agaris (doall (take n (drop start (mapcat #(get-agari' "1501/" %) ids))))]
     (time (agari-test agaris)))))

(defn one-day-test []
  (let [file (first (monthly-files "1401"))
        ids (get-ids-from-file file)
        agaris (mapcat #(get-agari' "1401/" %) ids)]
    (agari-test agaris)))

;; (monthly-test "1401")
(defn monthly-test [month-str]
  (let [files  (monthly-files month-str)
        ids    (mapcat get-ids-from-file files)
        agaris (mapcat #(get-agari' (str month-str "/") %) ids)]
    (time (agari-test agaris))))

(defn year-test []
  (let [ids    (mapcat get-ids-from-file file-names)
        agaris (mapcat #(get-agari' (get-path-from-id %) %) ids)]
    (agari-test agaris)))

(defn xml-file-test [id]
  (let [agaris (get-agari id)]
    (agari-test agaris)))

(defn agari->problem [{:keys [:hu' :ten' :yaku' :aux :ba :kyoku :ie :dora :ura-dora] :as agari}]
  (let [problem (str (hand-to-str agari)
                     ":" ba kyoku "局" ie "家"
                     (when aux (clojure.string/join "" (map (comp str second) aux)))
                     "ドラ" (to-str dora)
                     (when ura-dora (str "裏ドラ" (to-str ura-dora)))
                     )]
    {:problem problem
     :hu hu' :ten ten' :yaku yaku'
     :answer (first (nan-ten? problem))}))

(defn add-path-to-id [id]
  (str (extract-date id) "/" id))

(defn convert [agari]
  (with-open [fout (clojure.java.io/writer "problems.txt" :append true :encoding "UTF-8")]
    (.write fout (str (:problem (agari->problem agari)) "\n"))))

(defn start-convert []
  (let [ids (mapcat get-ids-from-file file-names)]
    (mapc' convert (take 1000 (mapcat (comp get-agari add-path-to-id) ids)))))




;; (defn agari-100-test2 []
;;   (let [files (monthly-files "1401")
;;         ids (mapcat get-ids-from-file files)
;;         agaris (take 1000 (mapcat #(get-agari' "1401/" %) ids))]
;;     (mapc-with-time
;;      (fn [agari]
;;        (let [p (agari->problem agari)]
;;          (when-not (= (extract-by p [:hu :ten]) [(get-in p [:answer :符]) (get-in p [:answer :点])])
;;            (println "\n" p))))
;;      agaris 100)))

'ok
