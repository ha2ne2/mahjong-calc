;;;; 2014-12-17
;;;; 麻雀あがり判定プログラム
;; (agari? "11122233344455m")
;; =>
;; ("123m123m123m444m55m"
;;  "111m234m234m234m55m"
;;  "111m234m345m345m22m"
;;  "111m222m333m444m55m")

;; 天和が出るまでひたすら配牌を繰り返す tenho-chllenge
;; (tenho-challenge)
;; =>
;; (:TENHO "3m7p7m7s1m8p6s北2m北9m8s9p8m"
;;  :FIGURE ("123m789m789p678s北北")
;;  :TRY 89941 :SEC 53 :TRY/SEC 1697)

;; 何待ちか調べる nanimati?
;; 九蓮9面待ち
;; (nanimati? "1112345678999m")
;; =>
;; (((m 1) ("123m111m456m789m99m"))
;;  ((m 2) ("111m345m678m999m22m"))
;;  ((m 3) ("123m345m678m999m11m"))
;;  ((m 4) ("111m234m456m789m99m"))
;;  ((m 5) ("111m234m678m999m55m"))
;;  ((m 6) ("123m456m678m999m11m"))
;;  ((m 7) ("111m234m567m789m99m"))
;;  ((m 8) ("111m234m567m999m88m"))
;;  ((m 9) ("123m456m789m999m11m")))
;; 何飜か調べる nan-han?
;; (nan-han? "11122233344455p")
;; =>
;; (("123p123p123p444p55p" 7 [一盃口 清一色])
;;  ("111p234p234p234p55p" 7 [一盃口 清一色])
;;  ("111p234p345p345p22p" 7 [一盃口 清一色])
;;  ("111p222p333p444p55p" 8 [対々和 清一色]))

(ns ha2ne2.mahjong-calc
  (#?(:clj :require :cljs :require-macros)
   [ha2ne2.macros :refer [ana-assoc if-let-it yakulist-to-set point-table-generator koutu-hu-calculate]])
  (:require ;[#?(:clj clojure.test :cljs cljs.test) :as test]
            [clojure.set]
            [clojure.string :refer [join split]]
            [ha2ne2.util :refer [atom? Y flatten1 empty-to-nil f s strict-take iterate*
                                 my-flatten map* repeat-cat =x with-key find-x remove-x
                                 remove-xs intersection set-difference subset? remove-duplicate
                                 find-duplicate count-if juxt* juxt-or juxt-cat remove-nth
                                 random-take iter-perm permutations sfirst $ count= c-to-i s-to-i
                                 =c find-if flip include?]]))
(def yaku-set
  (yakulist-to-set
   ((1 門前清自摸和 立直 槍槓 嶺上開花 海底撈月 河底撈魚 役牌 断幺九 一盃口 平和)
    (2 混全帯幺九 一気通貫 三色同順 両立直 三色同刻 三槓子 対々和 三暗刻 小三元 混老頭 七対子)
    (3 純全帯幺九 混一色 二盃口)
    (6 清一色)
    (13 天和 地和 大三元 四暗刻 字一色 緑一色 清老頭
        国士無双 大四喜 小四喜 四槓子 九蓮宝燈))))

(def menzen-nomi (set '(一盃口 平和 二盃口)))
(def kuisagari (set '(混全帯幺九 一気通貫 三色同順 純全帯幺九 混一色 清一色)))

(def routou (set '((m 1) (m 9) (p 1) (p 9) (s 1) (s 9))))
(def yakuhai (set '((白) (発) (中))))
(def huuhai (set '((東) (南) (西) (北))))
(def jihai  (clojure.set/union yakuhai huuhai))
(def yaochu (clojure.set/union routou jihai))
(def pai-order (into {} (map-indexed #(vector %2 %) '(m p s 東 南 西 北 白 発 中))))

(defn koutu-generator [n]
  (fn [pairs]
    (and (count= n pairs) (apply = pairs))))
(def toitu? (koutu-generator 2))
(def koutu? (koutu-generator 3))
(def kantu? (koutu-generator 4))

(defn syuntu? [[[x n] :as syuntu]]
  (and (not (some jihai syuntu))
       (= syuntu [[x n][x (+ 1 n)][x (+ 2 n)]])))

(defn pon? [lst] (when (koutu? lst) `(:pon ~lst)))
(defn ti-? [lst] (when (syuntu? lst) `(:ti- ~lst)))
(defn kan? [lst] (when (= 4 (count lst)) `(:kan ~lst)))

;;(remove-last-char "1234'");=> "1234"
(defn remove-last-char [s] (apply str (butlast s)))

;; (num-to-list "2340s")
;; ((s 2) (s 3) (s 4) (s 5 red))
(defn num-to-list [s]
  (let [ary (vec s)
        color (symbol (str (last ary)))]
    (map (fn [c]
           (let [n (with-key int - c \0)]
             (if (= n 0)
               (with-meta (list color 5) {:red true})
               (list color n))))
         (butlast ary))))

;; (to-list* "123m白白白")
;; ((m 1) (m 2) (m 3) (白) (白) (白))
(defn to-list* [s]
  (letfn [(conv [s]
            ((if (= (count s) 1)
               (comp list list symbol)  ; jihai 
               num-to-list) s))] ; suuhai
    (if (= (last s) \')
      (with-meta (mapcat conv (re-seq #"\d+\w|\W" (remove-last-char s)))
        {:kind :ankan})
      (mapcat conv (re-seq #"\d+\w|\W" s)))))
  
    
  
;; (concat (mapcat num-to-list (re-seq #"\d+\w" s)) 
;;         (map #(list (symbol %)) (re-seq #"\W" s)))) ; jihai 

(defn naki-to-list [s]
  (if (= (last s) \')
    `(:ankan ~(to-list* (remove-last-char s)))
    ((juxt-or pon? ti-? kan?) (to-list* s))))

(defn tumo-or-ron [s]
  (if (empty? s)
    nil
    (if (= (last s) \')
      `(:tumo ~@(to-list* (remove-last-char s)))
      `(:ron ~@(to-list* s)))))

;; "12344678m34789s 2s:東4局1本場東家立直一発ドラ7m" フォーマットの拡張
(def sample "12344678m34789s 2s:東4局1本場東家立直一発ドラ7m8m白裏ドラ8m7s発")

;; (nan-ten? "12344678m34789s 2s':東4局1本場東家立直一発嶺上ドラ7m8m裏ドラ8m7s")
;; (("123m678m234s789s44m" :符 20 :飜 9 :点 24000 :役 ((1 立直) (1 一発) (1 嶺上開花) (1 平和) (1 門前自摸) (2 ドラ) (2 裏ドラ))))


;; {:ba 東, :kyoku 4, :honba 1, :ie 東, :ri-ti true, :ippatu false, :dora ((m 7) (m 8) (白)), :ura-dora ((m 8) (s 7) (発)), :oya true}
;; (read-info "12344678m34789s 2s")
;; {:ba 東, :kyoku 1, :honba 0, :ie 南, :ri-ti false, :ippatu false, :dora nil, :ura-dora nil, :oya false}
(defn read-info [info]
  (let [info (if info info "")
        oya (re-find #"親" info)
        ;; 東n局、南n局の抽出
        ba (re-find #"(東|南|西|北)(\d)局" info)
        ;; ;; n本場の抽出
        honba (re-find #"(\d+)本場" info)
        ;; n家の抽出
        ie (re-find #"(.)家" info)
        ;; 立直一発の抽出
        ;; ri-ti (re-find #"立直" info)
        ;; ippatu (re-find #"一発" info)
        ;; ドラの抽出
        dora (re-find #"ドラ(.+?)($|裏)" info)
        ura-dora (re-find #"裏ドラ(.+)$" info)
        
        ;; 立直 一発 嶺上開花 海底撈月 河底撈魚
        aux (concat
             (when (re-find #"立直" info)
               (if (re-find #"両立直" info)
                 '((2 両立直)) '((1 立直))))
             (when (re-find #"一発" info) '((1 一発)))
             (when (re-find #"嶺上" info) '((1 嶺上開花)))
             (when (re-find #"槍槓" info) '((1 槍槓)))
             (when (re-find #"海底" info) '((1 海底撈月)))
             (when (re-find #"河底" info) '((1 河底撈魚))))
        ]
    (array-map
     :ba (if ba (symbol (ba 1)) '東) :kyoku (if ba (s-to-i (ba 2)) 1)
     :honba (if honba (s-to-i (honba 1)) 0) :ie (if ie (symbol (ie 1)) '南)
     :dora (when dora (to-list* (dora 1)))
     :ura-dora (when ura-dora (to-list* (ura-dora 1)))
     :oya (true? (or (and ie (= "東" (ie 1))) oya))
     :aux aux)
    ))

(defn concat-array-map [a b]
  (apply array-map (flatten1 (concat (seq a) (seq b)))))

;; string -> [pai]
(defn to-list [s]
  (let [[tehai info] (split s #":")]
    (let [[a & b] (split tehai #" |_")]
      (let [akadora (map (comp symbol second) (re-seq #"0.*?([mps])" s))
            info (read-info info)
            menzen (to-list* a)
            naki (map to-list* (butlast b))
            hu-ro (map naki-to-list (butlast b)) ;; 改良の余地がある また今度
            ho-ra (tumo-or-ron (last b))
            hand (array-map :menzen menzen :naki naki :aka akadora :string s)
            hand2 (if hu-ro (reduce (fn [m naki] (ana-assoc m (first naki) (conj it (second naki))))
                                    hand
                                    hu-ro)
                      hand)
            hand3 (if ho-ra (concat-array-map
                             hand2
                             (array-map (first ho-ra) (second ho-ra)))
                      hand2)]
        ;; into を使うとhash-mapに変換されてしまう
        (concat-array-map info hand3)
        ))))



;; (group-pais (random-hand))
;; =>(((m 1) (m 1) (m 3) (m 3) (m 4)) ((p 1) (p 2) (p 3)) ((s 1) (s 2) (s 5) (s 6) (s 9)) () () ((西)) () () () ())
(defn group-pais [hand]
  (let [s (group-by #(first %) hand)]
    (map #(sort-by second (s %)) '(m p s 東 南 西 北 白 発 中))))

;; (sort-hand (to-list "1m2s3p2m3m4p2s白2p発白"))
;; =>((m 1) (m 2) (m 3) (p 3) (p 4) (p 2) (s 2) (s 2) (白) (白) (発))
(defn sort-hand [hand] (apply concat (group-pais hand)))

(def all-pais
  (sort-hand
   (concat (repeat-cat 4 (mapcat (fn [x] (map #(list x %) (range 1 10))) '(m p s)))
           (repeat-cat 4 '((東) (南) (西) (北) (白) (発) (中))))))

;(sort-grouped-hand '(((m 2) (m 3) (m 4)) ((p 7) (p 8) (p 9)) ((p 4) (p 5) (p 6)) ((s 6) (s 7) (s 8)) ((西) (西))))
;=>(((m 2) (m 3) (m 4)) ((p 4) (p 5) (p 6)) ((p 7) (p 8) (p 9)) ((s 6) (s 7) (s 8)) ((西) (西)))
(defn sort-grouped-hand [grouped-hand]
  (sort-by (juxt #(* -1 (count %)) #(pai-order (ffirst %)) sfirst) ; ソート優先度は 牌の数（雀頭を後ろに）、牌の種類、牌の数値
           grouped-hand))

;; (next-pai '(p 1))
;; -> (p 2)
(defn next-pai [p]
  (cond (< (count p) 2) nil
        (<= 9 (second p)) nil
        :else (list (first p) (inc (second p)))))

;; (to-str '((p 9) (m 4) (s 5) (s 4) (s 7) (m 7) (m 7) (p 1) (p 5) (m 6) (m 3) (北) (m 4) (p 1)))
;;=> "9p4m547s77m15p63m北4m1p"
(defn to-str [hand]
  (letfn [(rec [[[curr-color :as h] & t :as hand] [[prev-color] :as buf] result]
            (if (empty? hand)
              (str result (apply str (map second buf)) prev-color)
              (let [h (if ((comp :red meta) h) (list curr-color 0) h)]
                (if (and (not (jihai h)) (= curr-color prev-color))
                  (recur t (conj buf h) result)
                  (recur t [h]
                         (str result (apply str (map second buf)) prev-color))))))]
    (str (rec hand [] "")
         (when (= :ankan ((comp :kind meta) hand))
           "'"))))

(defn random-hand-str [] (to-str (random-take 14 all-pais)))

;; (pair-to-str '((m 1) (m 2) (m 3)))
;;=> "123m"
(defn pair-to-str [lst]
  (str (if (jihai (first lst))
         (if (= (count lst) 5)
           (str (apply str (repeat 4 (ffirst lst))) "'")
           (apply str (repeat (count lst) (ffirst lst))))
         (str (apply str (map second lst)) (ffirst lst)))))

(defn figure-to-str [{:keys [figure naki] :as hand}]
  (str (apply str (map pair-to-str figure))
       (when (not (empty? naki))
         (apply str " " (interpose " " (map pair-to-str naki))))))

;; 関数を返す関数に関数を返す関数を適用して関数を返す関数
(defn get-pairs-function-generator [pair-gen]
  (fn [lst]
    (remove-duplicate
     (for [x lst
           :let [a (pair-gen x)]
           :when (include? a lst)]
       a))))

;; (get-toitu (to-list* "123s333456m88p白白白"))
;; (((白) (白)) ((p 8) (p 8)) ((m 3) (m 3)))
(def get-toitu
  (get-pairs-function-generator
   (fn [pai] (repeat 2 pai))))

;; (get-koutu (to-list* "123s333456m88p白白白"))
;; (((白) (白) (白)) ((m 3) (m 3) (m 3)))
(def get-koutu
  (get-pairs-function-generator
   (fn [pai] (repeat 3 pai))))

;; (get-syuntu (to-list* "123s333456m88p白白白"))
;; (((m 3) (m 4) (m 5))
;;  ((m 4) (m 5) (m 6))
;;  ((s 1) (s 2) (s 3)))
(def get-syuntu
  (get-pairs-function-generator
   (fn [pai] (strict-take 3 (iterate* next-pai pai)))))

;; (get-syuntu-or-koutu (to-list* "111222333s"))
;; (((s 1) (s 2) (s 3))
;;  ((s 3) (s 3) (s 3))
;;  ((s 2) (s 2) (s 2))
;;  ((s 1) (s 1) (s 1)))
(def get-syuntu-or-koutu (juxt-cat get-syuntu get-koutu))


;; 上がり形かどうかの判定のコア関数
;; 順子か刻子をあらゆる組み合わせで取って行って、最後に2つ雀頭ができたらアガリ。
(defn figure-out [hand]
  (let [lst (if-let [ho-ra (or (hand :tumo) (hand :ron))]
              (conj (hand :menzen) ho-ra)
              (hand :menzen))]
    (letfn [(kokusi? [lst]
              (when (and (subset? lst (seq yaochu)) ;; ヤオチュウ全種が手配に含まれているか？
                         (subset? (seq yaochu) lst)) ;; かつ手配は全てヤオチュウか？→なら国士
                (list (map list lst))))
            (ti-toitu?
              ([lst] (ti-toitu? lst nil))
              ([[head & tail :as lst] result]
               (cond (empty? lst) (when (= 7 (count result)) (list (reverse result)))
                     (find-x head tail) (recur (remove-x head tail) (cons (list head head) result))
                     :else nil)))
            
            (rec% [lst acc]
              (if (empty? lst)
                (list acc)
                (if-let [mentus (seq (get-syuntu-or-koutu lst))]
                  (mapcat #(rec% (remove-xs % lst) (cons % acc)) mentus)
                  nil)))

            (rec [lst]
              (if-let [atamas (seq (get-toitu lst))]
                (mapcat #(rec% (remove-xs % lst) (list %)) atamas)
                nil))]

      (->> (concat (kokusi? lst) (ti-toitu? lst) (rec lst))
           (map sort-grouped-hand)
           (#(remove-duplicate % set))
           (map #(concat-array-map hand {:figure %}))))))


;; (nanimati? "2223456778999p")
;; =>("2p" "5p" "6p" "7p" "8p" "9p")
;; (nanimati? "4555677889p発発発")
;; => ("3p" "4p" "6p" "9p")
;; (defn nanimati? [hand-str]
;;   (map #(to-str (list %))
;;        (filter #((complement empty?) (agari? (str hand-str (to-str (list %))))) (remove-duplicate all-pais))))



;; (waiting-shape (first-figure "22234p456p789p22s 2p"))
;; ((両面待ち "34p") (シャボ待ち "22p"))
;; (waiting-shape (first-figure "2234p456s456s789s 2p"))
;; ((両面待ち "34p") (単騎待ち "2p"))
(defn waiting-shape [{:keys [figure pon ti- kan ron tumo] :as hand}]
  (when-let [ho-ra (or ron tumo)]
    (letfn [(judge [shape]
              (let [lst (remove-x ho-ra shape)]
                (list (cond (= 1 (count lst)) '単騎待ち
                            (= (f lst) (s lst)) 'シャボ待ち
                            (or (= (next-pai (f lst)) (s lst)) (= (next-pai (s lst)) (f lst)))
                            (if (some routou lst) '辺張待ち '両面待ち)
                            :else '嵌張待ち)
                      (to-str lst))))]
      (map judge (filter #(find-x ho-ra %) figure)))))

(def first-figure (comp first figure-out to-list))

; (tanyao? (first-figure "234m345m678p345s22m")) ;=> (1 断么九)
(defn tanyao? [{:keys [figure pon ti- kan ankan] :as hand}]
  (let [all-pai (flatten1 (concat figure pon ti- kan ankan))]
    (when (every? (complement yaochu) all-pai)
      '(1 断么九))))
  

(defn -pinhu? [{:keys [figure pon ti- kan ankan ba ie] :as hand}]
  (and (= (count-if syuntu? (concat figure ti-)) 4)
       ((complement (clojure.set/union yakuhai (set `((~ba) (~ie)))))
        (ffirst (filter toitu? figure)))
       (find-x '両面待ち (flatten1 (waiting-shape hand)))))

;; 門前、全部順子、雀頭が数牌、両面待ち、
;; (pinhu? (first-figure "34s345p456s789m77s 2s")) ;=>  (1 平和)
;; (pinhu? (first-figure "34s345p456s789m白白 2s")) ;=> nil
;; (pinhu? (first-figure "34s345p444s789m77s 2s")) ;=> nil
;; (pinhu? (first-figure "34s345p789m77s 456s 2s")) ;=> nil
(defn pinhu? [{:keys [figure pon ti- kan ankan ba ie] :as hand}]
  (when (and (not (or pon ti- kan ankan))
             (-pinhu? hand))
    '(1 平和)))

; 符計算のためにだけ存在
(defn kui-pinhu? [{:keys [figure pon ti- kan ankan] :as hand}]
  (when (and (or pon ti- kan ankan)
             (-pinhu? hand))
    true))

;; 自摸の条件は、門前でツモアガリ
(defn tumo? [{:keys [figure pon ti- kan ankan tumo] :as hand}]
  (when (and (not (or pon ti- kan)) tumo)
    '(1 門前自摸)))

;; (iipeikou-or-ryanpeikou? (second (figure-out (to-list "123p123p456s白456s 白"))))
;; (3 二盃口)
(defn iipeikou-or-ryanpeikou? [{:keys [figure pon ti- kan] :as hand}]
  (when (not (or pon ti- kan))
    (let [n (count (find-duplicate (filter syuntu? figure)))]
      (cond (= n 1) '(1 一盃口)
            (= n 2) '(3 二盃口)
            :else nil))))

;; (tu-i-so-? (first-figure "東東東南南南西西西北北発発発")) ;=> (13 字一色)
;; (tu-i-so-? (first-figure "東東東南南南西西西33p発発発"))  ;=> nil
(defn tu-i-so-? [{:keys [figure pon ti- kan ankan] :as hand}]
  (when (subset? (flatten1 (concat figure pon ti- kan ankan)) (seq jihai))
    '(13 字一色)))



;; (tyanta-juntyan-honrou-tinrou? (first-figure "123m123s789p白白白99m"))  ;=> (2 混全帯么九)
;; (tyanta-juntyan-honrou-tinrou? (first-figure "123m123s789p789m99m"))    ;=> (3 純全帯么九)
;; (tyanta-juntyan-honrou-tinrou? (first-figure "123m123s789p9m 789m 9m")) ;=> (2 純全帯么九)
;; (tyanta-juntyan-honrou-tinrou? (first-figure "111m111s999p999m発発")) ;=> (2 混老頭)
;; (tyanta-juntyan-honrou-tinrou? (first-figure "111m111s999p999m99m"))  ;=> (13 清老頭)
(defn tyanta-juntyan-honrou-tinrou? [{:keys [figure pon ti- kan ankan] :as hand}]
  (when (not (or (count= 14 figure) (tu-i-so-? hand)))
    (let [a (concat figure pon ti- kan ankan)]
      (cond (every? (partial every? routou) a) '(13 清老頭)
            (every? (partial every? yaochu) a) '(2 混老頭)
            (every? (partial some routou) a) (list (if (or pon ti- kan) 2 3) '純全帯么九)
            (every? (partial some yaochu) a) (list (if (or pon ti- kan) 1 2) '混全帯么九)
            :else nil))))

;; (yakuhai? (first-figure "発発発345m2m中中中 白白白白 2m")) ;=> (3 役牌)
(defn yakuhai? [{:keys [figure pon ti- kan ankan] :as hand}]
  (let [n (count-if (comp yakuhai first)
                    (concat (filter koutu? figure) pon kan ankan))]
    (when (not (= n 0))
      (list n '役牌))))

;; (toitoihou? (first-figure "22m44m66m22p33p44p88p")) ;=> nil
;; (toitoihou? (first-figure "発発発333m777s2m 白白白 2m")) ;=> (2 対々和)
(defn toitoihou? [{:keys [figure naki] :as hand}]
  (when (= (count-if (some-fn koutu? kantu?) (concat figure naki)) 4)
    '(2 対々和)))

; argが字牌の時どうなる？
(defn nisyoku-gen [[[x a][_ b][_ c]]]
  (map (fn [x] `((~x ~a) (~x ~b) (~x ~c))) (remove-x x '(m p s))))

;; (sansyoku? (first-figure "234m234p234s666p22m")) ;=> (2 三色同順)
;; (sansyoku? (first-figure "666p2m 234m 234p 234s 2m")) ;=> (1 三色同順)
(defn sansyoku? [{:keys [figure pon ti- kan] :as hand}]
  (when (some #(subset? (nisyoku-gen %) (concat figure ti-))
              (filter syuntu? (concat figure ti-)))
    (list (if (or pon ti- kan) 1 2) '三色同順)))

(defn nisyokudoukou-gen [[[x a]]]
  (map (fn [x] `((~x ~a) (~x ~a) (~x ~a))) (remove-x x '(m p s))))

;; (sansyokudoukou? (first-figure "333m333p333s88p23s 4s"))   ;=> (2 三色同刻)
;; (sansyokudoukou? (first-figure "333m333s88m44s 3333p 4s")) ;=> (2 三色同刻)
(defn sansyokudoukou? [{:keys [figure pon ti- kan ankan] :as hand}]
  (when (some #(<= 3 %)
              (vals (reduce #(ana-assoc % (sfirst %2) (if (not it) 1 (inc it)))
                            {}
                            (filter (every-pred (some-fn koutu? kantu?)
                                                (complement (comp jihai first)))
                                    (concat figure pon kan ankan)))))
    '(2 三色同刻)))

(defn ti-toitu? [{:keys [figure pon ti- kan] :as hand}]
  (when (= 7 (count figure)) '(2 七対子)))

;; (ittuu? (first-figure "123456789m発発 白白白 発")) ;=> (1 一気通貫)
;; (ittuu? (first-figure "123456789m発白白白発"))     ;=> (2 一気通貫)
;; (ittuu? (first-figure "456789m発 123m 発"))        ;=> (1 一気通貫)
(defn ittuu? [{:keys [figure pon ti- kan] :as hand}]
  (when (some #(subset? % (concat figure ti-))
            '((((m 1) (m 2) (m 3)) ((m 4) (m 5) (m 6)) ((m 7) (m 8) (m 9)))
              (((p 1) (p 2) (p 3)) ((p 4) (p 5) (p 6)) ((p 7) (p 8) (p 9)))
              (((s 1) (s 2) (s 3)) ((s 4) (s 5) (s 6)) ((s 7) (s 8) (s 9)))))
    (list (if (or pon ti- kan) 1 2) '一気通貫)))


;; (honnitu-tinnitu? (first-figure "1234567892m 白白白 2m"));=> (2 混一色)
;; (honnitu-tinnitu? (first-figure "1234567892m白白白 2m")) ;=> (3 混一色)
;; (honnitu-tinnitu? (first-figure "1234567892m 111m 2m"))  ;=> (5 清一色)
;; (honnitu-tinnitu? (first-figure "1234567892m111m 2m"))   ;=> (6 清一色)
;; (honnitu-tinnitu? (first-figure "南南南東東東中中中白白白発 発")) ;=> nil
(defn honnitu-tinnitu? [{:keys [figure pon ti- kan ankan] :as hand}]
  (let [all-pai (flatten1 (concat figure pon ti- kan ankan))
        suuhai (set-difference all-pai (seq jihai))
        kind (ffirst suuhai)]
    (when (and (not (empty? suuhai))
               (every? #(= (first %) kind) suuhai))
      (if (empty? (intersection (seq jihai) all-pai))
        (list (if (or pon ti- kan) 5 6) '清一色)
        (list (if (or pon ti- kan) 2 3) '混一色)))))

;; (syousangen-daisangen? (first-figure "白白白発発発中中888m345s"))  ;=> (2 小三元)
;; (syousangen-daisangen? (first-figure "白白白発発発中中中88m345s")) ;=> (13 大三元)
(defn syousangen-daisangen? [{:keys [figure pon ti- kan ankan] :as hand}]
  (when (and (not (count= 7 figure)) (not (count= 14 figure)))
    (let [target (filter (comp yakuhai first) (concat figure pon kan ankan))]
      (when (count= 3 target)
        (if (every? (some-fn koutu? kantu?) target)
          '(13 大三元)
          '(2 小三元))))))

;; (su-si-ho-? (first-figure "東東東南南南西西西北北北33m"))  ;=> (13 大四喜)
;; (su-si-ho-? (first-figure "東東東南南南西西西北北333m"))  ;=> (13 小四喜)
(defn su-si-ho-? [{:keys [figure pon ti- kan ankan] :as hand}]
  (when (and (not (count= 7 figure)) (not (count= 14 figure)))
    (let [target (filter (comp huuhai first) (concat figure pon kan ankan))]
      (if (count= 4 target)
        (let [koutu (count-if (some-fn koutu? kantu?) target)]
          (if (= koutu 4) '(13 大四喜) '(13 小四喜)))
        nil))))

;; (ryu-i-so-? (first-figure "234s444s666s888s発発")) ;=> (13 緑一色)
;; (ryu-i-so-? (first-figure "234s444s666s999s発発")) ;=> nil
(defn ryu-i-so-? [{:keys [figure pon ti- kan ankan] :as hand}]
  (when (subset? (flatten1 (concat figure pon ti- kan ankan)) '((s 2) (s 3) (s 4) (s 6) (s 8) (発)))
    '(13 緑一色)))

(defn kokusi? [{:keys [figure pon ti- kan] :as hand}]
  (when (= (count figure) 14) '(13 国士無双)))

;; (tyuuren? (first-figure "1112345678999p 5p"))   ;=> (13 九蓮宝燈)
;; (tyuuren? (first-figure "1112345678p 999p 5p")) ;=> nil
(defn tyuuren? [{:keys [figure pon ti- kan ankan] :as hand}]
  (let [flat-figure (flatten1 figure)]
    (letfn [(f [color]
              (and (every? #(= (first %) (ffirst flat-figure)) flat-figure)
                   (= 1 (count (remove-xs ((to-list (str "1112345678999" color)) :menzen)
                                          flat-figure)))))]
      (when (and (not (or pon ti- kan ankan)) (some f '(m p s)))
        '(13 九蓮宝燈)))))

;; (sankantu-su-kantu? (first-figure "1s 2222s 4444p 555s 白白白白 1s"))  ;=> (2 三槓子)
;; (sankantu-su-kantu? (first-figure "1s 2222s 4444p 5555s 白白白白 1s")) ;=> (13 四槓子)
(defn sankantu-su-kantu? [{:keys [figure pon ti- kan ankan] :as hand}]
  (cond (count= 3 (concat kan ankan)) '(2 三槓子)
        (count= 4 (concat kan ankan)) '(13 四槓子)))

(defn sannankou-su-ankou? [{:keys [figure pon ti- kan ankan tumo ron] :as hand}]
  (let [anko (concat (filter koutu? figure) ankan)
        anko-janai (filter (complement koutu?) figure)]
    (cond (count= 3 anko)
          (if (and ron (some #(find-x ron %) anko))
            (if (some #(find-x ron %) anko-janai) '(2 三暗刻) nil)
            '(2 三暗刻))
          (count= 4 anko)
          (if (and ron (some #(find-x ron %) anko)) '(2 三暗刻) '(13 四暗刻)))))




;; (tanyao? (first-figure "234m345m678p345s22m"))                 ;=>(1 断么九)
;; (iipeikou-or-ryanpeikou? (first-figure "123s123s456p789m33s")) ;=>(1 一盃口)
;; (iipeikou-or-ryanpeikou? (first-figure "123s123s456p456p33s")) ;=>(3 二盃口)
;; (tyanta-juntyan-honrou-tinrou? (first-figure "123m123s789p白白白99m")) ;=> (2 混全帯么九)
;; (tyanta-juntyan-honrou-tinrou? (first-figure "123m123s789p789m99m"))  ;=> (3 純全帯么九)
;; (tyanta-juntyan-honrou-tinrou? (first-figure "111m111s999p999m発発")) ;=> (2 混老頭)
;; (tyanta-juntyan-honrou-tinrou? (first-figure "111m111s999p999m99m"))  ;=> (13 清老頭)
;; (yakuhai? (first-figure "発発発345m678p白白白22m"))   ;=> (2 役牌)
;; (toitoihou? (first-figure "発発発333m777s白白白22m")) ;=> (2 対々和)
;; (sansyoku? (first-figure "234m234p234s666p22m"))    ;=>(2 三色同順)
;; (ti-toitu? (first-figure "22m33p44s66p22m白白白白")) ;=> (2 七対子)
;; (ittuu? (first-figure "123456789m白白白発発"))       ;=> (2 一気通貫)
;; (tinnitu? (first-figure "123456789111m22m"))        ;=>(6 清一色)
;; (syousangen-daisangen? (first-figure "白白白発発発中中888m345s"))  ;=> (2 小三元)
;; (syousangen-daisangen? (first-figure "白白白発発発中中中88m345s")) ;=> (13 大三元)
;; (sansyokudoukou? (first-figure "333m333p333s88m345s")) ;=> (2 三色同刻)
;; (su-si-ho-? (first-figure "東東東南南南西西西北北333m"))  ;=> (13 四喜和)
;; (tu-i-so-? (first-figure "東東東南南南西西西北北発発発"))  ;=> (13 字一色)
;; (ryu-i-so-? (first-figure "234s444s666s888s発発"))     ;=> (13 緑一色)
;; (tyuuren? (first-figure "11123455678999m"))           ;=> (13 九蓮宝燈)
;; (kokusi? (first-figure "19m19p19s東南西北白発中中"))    ;=> (13 国士無双)


;; (nan-han?% "11122233344455m")
;; (("123m123m123m444m55m" (1 一盃口) (6 清一色))
;;  ("111m234m234m234m55m" (1 一盃口) (6 清一色))
;;  ("111m234m345m345m22m" (1 一盃口) (6 清一色))
;;  ("111m222m333m444m55m" (2 対々和) (6 清一色) (13 四暗刻)))

;; (nan-han?% "1122335566778m 8m")
;; (("11m22m33m55m66m77m88m" (2 七対子) (6 清一色))
;;  ("123m123m567m567m88m" (3 二盃口) (6 清一色))
;;  ("123m123m678m678m55m" (1 平和) (3 二盃口) (6 清一色)))

;; (nan-han?% "111222333m7899s 9s")
;; (("123m123m123m789s99s" (1 平和) (1 一盃口) (3 純全帯么九))
;;  ("111m222m333m789s99s" (2 三暗刻)))

;; (nan-ten? "23345m567p67855p 4m")
;; (("234m345m567p678p55p" 40 2 2600 (1 平和) (1 断么九)))
;; これカンチャン待ちで見てるから40符になってる。修正が必要。

;; (nan-ten?* "1122335566778m 8m")
;; (("11m22m33m55m66m77m88m" :符 40 :飜 8 :点 16000 :役 ((2 七対子) (6 清一色)))
;;  ("123m123m567m567m88m" :符 40 :飜 9 :点 16000 :役 ((3 二盃口) (6 清一色)))
;;  ("123m123m678m678m55m" :符 30 :飜 10 :点 16000 :役 ((1 平和) (3 二盃口) (6 清一色))))


(defn total-han [lst]
  `(~(first lst)
    ~(reduce + (map first (rest (butlast lst))))
    ~@(rest lst)))

(defn nan-han? [figure]
  (let [yaku-list
        ((juxt* tanyao? pinhu? tumo? iipeikou-or-ryanpeikou? tyanta-juntyan-honrou-tinrou?
                yakuhai? toitoihou? sansyoku? ti-toitu? ittuu? honnitu-tinnitu? syousangen-daisangen?
                sansyokudoukou? sannankou-su-ankou? sankantu-su-kantu?
                su-si-ho-? tu-i-so-? ryu-i-so-? tyuuren? kokusi?)
         figure)
        {yakuman true yaku false} (group-by (comp (=c 13) first) yaku-list)
        yaku-list (if (empty? yakuman) yaku yakuman)
        han (reduce + (map first yaku-list))]
    (list han yaku-list)))

;; (defn nan-han? [str]
;;   (map total-han
;;        (map (juxt* figure-to-str tanyao? pinhu? tumo? iipeikou-or-ryanpeikou? tyanta-juntyan-honrou-tinrou?
;;                    yakuhai? toitoihou? sansyoku? ti-toitu? ittuu? honnitu-tinnitu? syousangen-daisangen?
;;                    sansyokudoukou? sannankou-su-ankou? sankantu-su-kantu? su-si-ho-? tu-i-so-? ryu-i-so-? tyuuren? kokusi?
;;                    identity)
;;             (figure-out (to-list str)))))

;; usage
;; (koutu-hu-calc pon 2 4)
;; (koutu-hu-calc kan 8 16)
;; (koutu-hu-calc ankan 16 32)
(defn koutu-hu-calc [triples chutyan-hu yaochu-hu]
  (reduce + (map #(if ((comp yaochu first) %) yaochu-hu chutyan-hu) triples)))


(defn get-toitu-from-figure [figure]
  (find-if toitu? figure))

(defn my-kiriage [n]
  (* 10 (int (/ (+ n 9) 10))))

(defn every-pred' [f & fs]
  (fn [x]
    (if (empty? fs)
      (f x)
      (and (f x)
           ((apply every-pred' fs) x)))))

(defn nan-pu?
  ([hand]
   (nan-pu? hand nil))

  ([{:keys [figure pon ti- kan ankan tumo ron ba ie] :as hand} debug-flag]
   (if (count= 14 figure)
     0 ; kokusi
     (let [{koutu true not-koutu false} (group-by koutu? figure)
           minko (if-let-it
                  (and (it-is (find-if #(find-x ron %) koutu))
                       (not (some (partial find-x ron) not-koutu)))
                  it nil)
           figure (if minko (remove-x minko figure) figure)
           pon'   (if minko (concat pon (list minko)) pon)

           hu-tei
           20
           
           agari-hu
           (cond (and (not (or pon ti- kan)) ron) 10 ; 門前加符
                 tumo 2 ; ツモ符
                 :else 0)

           koutu-hu
           (+
            (koutu-hu-calc pon' 2 4)
            (koutu-hu-calc (filter koutu? figure) 4 8)
            (koutu-hu-calc kan 8 16)
            (koutu-hu-calc ankan 16 32))
           
           atama-hu
           (let [atama (first (get-toitu-from-figure figure))]
             (+ (if (= (list ba) atama) 2 0)
                (if (= (list ie) atama) 2 0)
                (if (yakuhai atama) 2 0)))
           
           mati-hu
           (if (intersection
                '(辺張待ち 嵌張待ち 単騎待ち)
                (map first (waiting-shape hand)))
             2 0)]
       (if debug-flag
         (list '(+ 副底 門前加符 刻子符 頭符 待ち符)
               (list '+ hu-tei agari-hu koutu-hu atama-hu mati-hu)
               minko
               pon
               figure)
         (my-kiriage (+ hu-tei agari-hu koutu-hu atama-hu mati-hu)))
       ))))


(defn show-figure [figure]
  (array-map
   :手 (figure-to-str figure)
   :親/子 (if (figure :oya) '親 '子)
   :符 (figure :hu)
   :飜 (figure :han)
   :点 (figure :ten)
   :役 (figure :yaku)))

;; (nan-ten? "12344678m34789s 2s:東4局1本場東家立直一発ドラ7m裏ドラ4m")
;; ({:手 "123m678m234s789s44m ", :親/子 親, :符 30, :飜 6, :点 18000, :役 ((1 立直) (1 一発) (1 平和) (1 ドラ) (2 裏ドラ))})
;; (nan-ten? "1122334455667m 7m")
;; ({:手 "123m123m567m567m44m ", :親/子 子, :符 30, :飜 10, :点 16000, :役 ((1 平和) (3 二盃口) (6 清一色))}
;;  {:手 "234m234m567m567m11m ", :親/子 子, :符 30, :飜 10, :点 16000, :役 ((1 平和) (3 二盃口) (6 清一色))}
;;  {:手 "123m123m456m456m77m ", :親/子 子, :符 40, :飜 9, :点 16000, :役 ((3 二盃口) (6 清一色))}
;;  {:手 "11m22m33m44m55m66m77m ", :親/子 子, :符 25, :飜 8, :点 16000, :役 ((2 七対子) (6 清一色))})

(defn nibai [n] (* n 2))

(defn my-ceil [n]
  (* 100 (int (/ (+ n 99) 100))))

;; (nan-ten?* 30 4 false false)
;; 7700
(defn calc-ten [hu han oya tumo]
  (cond
    (= han 0)
    0
    (<= han 5)
    (let [base (* hu (int (Math/pow 2 (+ han 2))))
          tumo-ko-pay (my-ceil base)
          tumo-oya-pay (my-ceil (nibai base))]
      (if (< base 2000)
        (if tumo
          (if oya
            (* tumo-oya-pay 3)
            (+ (nibai tumo-ko-pay) tumo-oya-pay))
          (my-ceil (* base (if oya 6 4))))
        (if oya 12000 8000))
        ; (if tumo "4000all" "12000") (if tumo "2000-4000" "8000")
      )
    (<= han 7) ;haneman
    (if oya 18000 12000)
    ; (if tumo "6000all" "18000") (if tumo "3000-6000" "12000")
    (<= han 10) ;baiman
    (if oya 24000 16000)
    ; (if tumo "8000all" "24000") (if tumo "4000-8000" "16000")
    (<= han 12) ;sanbaiman
    (if oya 36000 24000 )
    ; (if tumo "12000all" "36000") (if tumo "6000-12000" "24000")
    (<= 13 han 25) ;yakuman
    (if oya 48000 32000)
    (<= 26 han 38) ;double yakuman
    (if oya 96000 64000)
    (<= 39 han) ;triple yakuman
    (if oya 144000 96000)
    ; (if tumo "16000all" "48000") (if tumo "8000-16000" "32000")
    ))


(defn count-dora [dora-lst hand]
  (reduce + (map #(count-if (=c %) hand) dora-lst)))

;; (nan-ten? "234s345m6667899p 9p':一発ドラ9999p")
;; (("345m678p999p234s66p" :符 30 :飜 10 :点 16000 :役 ((1 一発) (1 門前自摸) (8 ドラ))) ("345m666p789p234s99p" :符 30 :飜 10 :点 16000 :役 ((1 一発) (1 門前自摸) (8 ドラ))))

(defn nan-ten?% [lst]
  (let [figures (figure-out lst)
        hans (map nan-han? figures)
        hus  (map nan-pu? figures)]
    (letfn [(add-info [figure [han yaku] hu]
              (let [dora-count (count-dora (figure :dora)
                                           (concat (flatten1 (figure :figure))
                                                   (flatten1 (figure :naki))))
                    ura-dora-count (count-dora (figure :ura-dora)
                                               (concat (flatten1 (figure :figure))
                                                       (flatten1 (figure :naki))))
                    aka-dora-count (count (figure :aka))
                    yakuman-flag (some (comp (=c 13) first) yaku)
                    yaku (concat
                          (figure :aux)
                          yaku
                          (when (not yakuman-flag)
                            (concat
                             (when (some (comp (=c (figure :ba)) ffirst)
                                         (concat (filter koutu? (figure :figure))
                                                 (figure :naki)))
                               '((1 場風牌)))
                             (when (some (comp (=c (figure :ie)) ffirst)
                                         (concat (filter koutu? (figure :figure))
                                                 (figure :naki)))
                               '((1 自風牌)))
                             (when (not= dora-count 0) `((~dora-count ~'ドラ)))
                             (when (not= ura-dora-count 0) `((~ura-dora-count ~'裏ドラ)))))
                             (when (not= aka-dora-count 0) `((~aka-dora-count ~'赤ドラ)))
                          )
                    han (reduce + (map first yaku))
                    yaku-lst (map second yaku)
                    hu
                    (if (find-x '平和 yaku-lst)
                      (if (find-x '門前自摸 yaku-lst) 20 30) ; ピンフツモは20符
                      (cond (kui-pinhu? figure) 30 ; 喰いピンフは30符
                            (find-x '七対子 yaku-lst) 25
                            :else hu))
                    ten (calc-ten hu han (figure :oya) (figure :tumo))]
                (assoc figure
                       :han han
                       :yaku yaku
                       :hu hu
                       :ten ten)
                ))]
      (sort-by (juxt :飜 :符)
               (flip compare)
               (map (comp show-figure add-info) figures hans hus)))))



(defn nan-ten? [str]
  (nan-ten?% (to-list str)))

;; (agari? "111p234p345p345p22p")
;;=> ("123p123p123p444p55p" "111p234p345p345p22p" "111p234p234p234p55p" "111p222p333p444p55p")
(defn agari? [hand-str] (nan-ten? hand-str))



;; to-show


(defn pinch [f first last] #(apply f `(~first ~@%& ~last)))

;; {東 "z1.png", 南 "z2.png", 西 "z3.png", 北 "z4.png", 白 "z5.png", 発 "z6.png", 中 "z7.png"}
(def jihai-html
  (apply array-map
   (apply concat (map-indexed #(list %2 (str "z" (+ 1 %1) ".png")) '(東 南 西 北 白 発 中)))))

;; (pai-to-file-name '(白)) ;=> "z5.png"
(defn pai-to-file-name [[color n :as pai]]
  (if (jihai pai)
    (jihai-html color)
    (if (:red (meta pai))
      (str color n "a.png")
      (str color n ".png"))))

(def pai_image_yoko_path "./resources/pai_image_yoko/")
(def pai_image_path "./resources/pai_image/")

(def add-path (partial partial str))
(def add-img-tag (pinch str "<img src='" "'>"))

(def naki-to-tag #(str "<img src='" pai_image_yoko_path (pai-to-file-name %) "'>"))
;; (def naki-to-tag (comp add-img-tag (add-path pai_image_yoko_path) pai-to-file-name))
(def pai-to-tag (comp add-img-tag (add-path pai_image_path) pai-to-file-name))
;; (def pais-to-tags (comp (partial reduce str) (partial map pai-to-tag)))

(defn pais-to-tags [pais]
  (reduce str (map
               #(str "<img src='" pai_image_path (pai-to-file-name %) "'>")
               pais)))

(defn naki-to-tags [[head & tail :as unit]]
  (if (count= 5 unit)
    (str ((comp add-img-tag (add-path pai_image_path)) "ura.png")
         (pai-to-tag head)
         (pai-to-tag head)
         ((comp add-img-tag (add-path pai_image_path)) "ura.png"))
    (str (naki-to-tag head) (pais-to-tags tail))))
              
(defn hand-to-html [{:keys [menzen naki ron tumo] :as hand}]
  (let [menzen (pais-to-tags menzen)]
    (str menzen
         (when naki (str "　" (join "　" (map naki-to-tags naki))))
         (when (or ron tumo) (str "　" (pai-to-tag (or ron tumo)))))))


(defn hand-to-str [{:keys [menzen naki ron tumo] :as hand}]
  (let [menzen (to-str menzen)]
    (str menzen
         (when naki (str " " (join " " (map to-str naki))))
         (when (or ron tumo)
           (str " " (to-str (list (or ron tumo)))
                (when tumo "'")))
         )))

;; (to-list "1123406m 789m 4444m' 1m")
;; {:ba 東, :kyoku 1, :honba 0, :ie 南, :dora nil, :ura-dora nil, :oya false, :aux (), :menzen ((m 1) (m 1) (m 2) (m 3) (m 4) (m 5) (m 6)), :naki (((m 7) (m 8) (m 9)) ((m 4) (m 4) (m 4) (m 4) ('))), :aka (m), :string "1123406m 789m 4444m' 1m", :ti- (((m 7) (m 8) (m 9))), :ankan (((m 4) (m 4) (m 4) (m 4))), :ron (m 1)}



(defn pretty [sexp]
  (if (not (count= 1 sexp))
    ((pinch str "(" ")") (join "<br>" sexp))
    (str sexp)))

(defn nan-ten?-with-img [s]
  (let [figures (nan-ten? s)]
    (str (hand-to-html (to-list s)) "<br><br>"
         (apply str (interpose "<br>" figures)))))

;; (nan-ten? "34578m345p33345s 6m':東4局南家立直ドラ中裏ドラ発")
;; ({:手 "345m678m345p345s33s", :親/子 子, :符 20, :飜 6, :点 12000, :役 ((1 立直) (1 断么九) (1 平和) (1 門前自摸) (2 三色同順))})

;; (子 20 12000)
(defn choices-generator [s]
  (let [{ten :点} (first (nan-ten? s))]
    (shuffle 
     (conj (repeatedly 3 #(calc-ten (first (random-take 1 [25 30 40 50 60]))
                                    (inc (rand-int 4))
                                    (first (random-take 1 [true false]))
                                    (first (random-take 1 [true false]))))
           ten))))

#?(:clj
   (defn tenho-challenge []
     (letfn [(challenge [try]
               (let [hand (to-list (random-hand-str))]
                 (when (zero? (mod try 1000)) (println try (to-str (:menzen hand))))
                 (if-let [figure (seq (figure-out hand))]
                   (do (println hand (show-figure (first figure)))
                       (list (first figure) (inc try)))
                   (recur (inc try)))))]
       (let [start (System/currentTimeMillis)]
         (let [[fig try] (challenge 0)]
           (let [end (System/currentTimeMillis)
                 elapsed (int (/ (- end start) 1000))]
             `(:TENHO ~(:string fig) :FIGURE ~(figure-to-str fig)
                      :TRY ~try :SEC ~elapsed  :TRY/SEC ~(int (/ try elapsed)))))))))

'ok



