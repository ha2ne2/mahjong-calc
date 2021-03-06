(ns ha2ne2.macros)

(defmacro ana-assoc [m key val]
  `(let [~'it (~m ~key)]
     (assoc ~m ~key ~val)))

;; (tree-find-if (every-pred coll? #(= 3 (count %)))
;;  '(((1)) 1 1 1 (((1 1 5) (1)) 1) ((((1)) (2)))))
;;=> (1 1 5)

;; (tree-find-if (every-pred number? even?)
;;  '(((1)) 1 1 1 (((1 1 5) (1)) 1) ((((1)) (2)))))
;;=> 2
(defn tree-find-if [f tree]
  (letfn [(rec [f tree]
            (cond (f tree) tree
                  (or (not (coll? tree)) (empty? tree)) nil
                  :else (or (rec f (first tree)) (rec2 f (rest tree)))))
          (rec2 [f tree]
            (if (empty? tree) nil
                (or (rec f (first tree)) (rec2 f (rest tree)))))]
    (rec f tree)))

(defn tree-copy [tree]
  (cond (nil? tree) nil
        ((every-pred coll? empty?) tree) '()
        (coll? tree) (cons (tree-copy (first tree)) (tree-copy (rest tree)))
        :else tree))

;; (tree-replace-if (every-pred number? even?) '_ '(1 ((2) (3)) (((4) 5) 6) [7] [8] ((((9 10))))))
;; (1 ((_) (3)) (((_) 5) _) (7) (_) ((((9 _)))))
(defn tree-replace-if [f x tree]
  (letfn [(rec [f tree]
            (cond (f tree) x
                  (or (not (coll? tree)) (empty? tree)) tree
                  :else (cons (rec f (first tree)) (rec2 f (rest tree)))))
          (rec2 [f tree]
            (if (empty? tree) tree
                (cons (rec f (first tree)) (rec2 f (rest tree)))))]
    (rec f tree)))

;; (if-let-it (= 15 (it-is (* 3 5)))
;;   (list 'it 'is it)
;;   nil)
;; ↓
;; (let [it (* 3 5)]
;;   (if (= 15 it)
;;     (list 'it 'is it)
;;     nil))
;; ↓
;;(it is 15)
(defmacro if-let-it [condition then-clause else-clause]
  (let [it-exp (tree-find-if (every-pred coll? (comp #(= 'it-is %) first)) condition)
        cond (tree-replace-if #(= it-exp %) 'it condition)]
    `(let [~'it ~(second it-exp)]
       (if ~cond
         ~then-clause
         ~else-clause))))



;; (yakulist-to-set
;;    ((1 門前清自摸和 立直 槍槓 嶺上開花 海底摸月 河底撈魚 役牌 断幺九 一盃口 平和)
;;     (2 混全帯幺九 一気通貫 三色同順 両立直 三色同刻 三槓子 対々和 三暗刻 小三元 混老頭 七対子)
;;     (3 純全帯幺九 混一色 二盃口)
;;     (6 清一色)
;;     (13 天和 地和 大三元 四暗刻 字一色 緑一色 清老頭
;;         国士無双 大四喜 小四喜 四槓子 九蓮宝燈)))
;; ↑が↓
;; {門前清自摸和 1, 立直 1, 槍槓 1, 嶺上開花 1, 海底摸月 1, 河底撈魚 1, 役牌 1, 断幺九 1, 一盃口 1, 平和 1,
;;  混全帯幺九 2, 一気通貫 2, 三色同順 2, 両立直 2, 三色同刻 2, 三槓子 2, 対々和 2, 三暗刻 2, 小三元 2, 混老頭 2, 七対子 2,
;;  純全帯幺九 3, 混一色 3, 二盃口 3,
;;  清一色 6,
;;  天和 13, 地和 13, 大三元 13, 四暗刻 13, 字一色 13, 緑一色 13, 清老頭 13, 国士無双 13, 大四喜 13, 小四喜 13, 四槓子 13, 九蓮宝燈 13}
(defmacro yakulist-to-set [lst]
  (letfn [(f [clause]
            (apply concat (map #(list `(quote ~%) (first clause)) (rest clause))))]
    `(array-map ~@(mapcat f lst))))

;; (point-table-generator
;;    ((700  1000 1300 1600 2000 2300 2600 2900 3200 3600)
;;     (1300 2000 2600 3200 3900 4500 5200 5800 6400 7100)
;;     (2600 3900 5200 6400 7700 8000 8000 8000 8000 8000)
;;     (5200 7700 8000 8000 8000 8000 8000 8000 8000 8000)
;;     8000
;;     12000 12000
;;     16000 16000 16000
;;     24000 24000
;;     32000))
;; ↑が↓になる
;;   {1 {20 700, 30 1000, 40 1300, 50 1600, 60 2000, 70 2300, 80 2600, 90 2900, 100 3200, 110 3600},
;;    2 {20 1300, 30 2000, 40 2600, 50 3200, 60 3900, 70 4500, 80 5200, 90 5800, 100 6400, 110 7100},
;;    3 {20 2600, 30 3900, 40 5200, 50 6400, 60 7700, 70 8000, 80 8000, 90 8000, 100 8000, 110 8000},
;;    4 {20 5200, 30 7700, 40 8000, 50 8000, 60 8000, 70 8000, 80 8000, 90 8000, 100 8000, 110 8000},
;;    5 {20 8000, 30 8000, 40 8000, 50 8000, 60 8000, 70 8000, 80 8000, 90 8000, 100 8000, 110 8000},
;;    6 {20 12000, 30 12000, 40 12000, 50 12000, 60 12000, 70 12000, 80 12000, 90 12000, 100 12000, 110 12000},
;;    7 {20 12000, 30 12000, 40 12000, 50 12000, 60 12000, 70 12000, 80 12000, 90 12000, 100 12000, 110 12000},
;;    8 {20 16000, 30 16000, 40 16000, 50 16000, 60 16000, 70 16000, 80 16000, 90 16000, 100 16000, 110 16000},
;;    9 {20 16000, 30 16000, 40 16000, 50 16000, 60 16000, 70 16000, 80 16000, 90 16000, 100 16000, 110 16000},
;;    10 {20 16000, 30 16000, 40 16000, 50 16000, 60 16000, 70 16000, 80 16000, 90 16000, 100 16000, 110 16000},
;;    11 {20 24000, 30 24000, 40 24000, 50 24000, 60 24000, 70 24000, 80 24000, 90 24000, 100 24000, 110 24000},
;;    12 {20 24000, 30 24000, 40 24000, 50 24000, 60 24000, 70 24000, 80 24000, 90 24000, 100 24000, 110 24000},
;;    13 {20 32000, 30 32000, 40 32000, 50 32000, 60 32000, 70 32000, 80 32000, 90 32000, 100 32000, 110 32000}}
(defmacro point-table-generator [clauses]
  (let [hu (map #(* % 10) (range 2 12))]
    (cons 'array-map
          (mapcat list
                  (range 1 (+ 1 (count clauses)))
                  (map (fn [clause]
                         (cons 'array-map
                               (mapcat list hu
                                       (if (coll? clause)
                                         clause
                                         (repeat (count hu) clause)))))
                       clauses)))))

;; (koutu-hu-calculate
;;  (pon 2 4)
;;  ((filter koutu? figure) 4 8)
;;  (kan 8 16)
;;  (ankan 16 32))
;; ↓
;; (+
;;  (koutu-hu-calc pon 2 4)
;;  (koutu-hu-calc (filter koutu? figure) 4 8)
;;  (koutu-hu-calc kan 8 16)
;;  (koutu-hu-calc ankan 16 32))
(defmacro koutu-hu-calculate [& clauses]
  (letfn [(body-gen [clause] `(~'koutu-hu-calc ~@clause))]
    `(+ ~@(map body-gen clauses))))

