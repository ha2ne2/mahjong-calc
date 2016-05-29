(ns ha2ne2.util
  (#?(:clj :require :cljs :require-macros)
   [ha2ne2.macros :refer [ana-assoc if-let-it yakulist-to-set point-table-generator koutu-hu-calculate]]))

;;;;;;; util
(def atom? (complement seq?))
(defn Y [f] ((fn [g] (g g)) (fn [g] (f #(g g)))))
(def flatten1 (partial mapcat identity))
(defn empty-to-nil [lst] (if (empty? lst) nil lst))
(def f first)
(def s second)

(defn strict-take [n lst]
  (let [xs (take n lst)]
    (if (= n (count xs)) xs nil)))

(defn iterate* [f x]
  (if (nil? x) nil
    (cons x (lazy-seq (iterate* f (f x))))))

(defn my-flatten
  ([lst] (my-flatten lst nil))
  ([lst result]
   (cond (= () lst) result
         (atom? lst) (cons lst result)
         true (my-flatten (first lst) (my-flatten (rest lst) result)))))

; remove nil from (map f lst). alternative mapcat
(defn map* [f [head & tail :as lst]]
  (if (empty? lst)
    nil
    (if-let [x (f head)]
      (cons x (lazy-seq (map* f tail)))
      (lazy-seq (map* f tail)))))


;; (repeat-cat 3 '((白) (発)))
;; => ((白) (発) (白) (発) (白) (発))
(defn repeat-cat [n lst] (apply concat (repeat n lst)))

;; (remove (=x 1) '(1 2 3 1 2 3))
;; -> (2 3 2 3)
(defn =x [x & {:keys [then else]}]
  (fn [y] (if (= x y)
            (if then (then x y) x)
            (if else (else x y) nil))))

;; (+ (count '(1 2 3) (count '(4 5 6))
;; (with-key count + '(1 2 3) '(4 5 6))
;; -> 6
(defn with-key [key f & args]
  (reduce f (map key args)))

;; (map-reduce char-to-int #(+ (* % 10) %2) s)
;; (reduce #(+ (* % 10) %2) (map char-to-int s))
(defn map-reduce [map-fn reduce-fn xs]
  (reduce reduce-fn (map map-fn xs)))

;; (find-x 'c '(a b c d e))
;; -> c
(defn find-x [x lst]
  (if (empty? lst)
    nil
    (if (= x (first lst)) x
        (recur x (rest lst)))))

;; (remove-x 'a '(a b a b a b))
;; (b a b a b)
(defn remove-x
  ([x lst] (remove-x x lst 1))
  ([x lst n]
     (if (or (empty? lst) (= n 0))
       lst
       (if (= x (first lst))
         (lazy-seq (remove-x x (rest lst) (dec n)))
         (cons (first lst) (lazy-seq (remove-x x (rest lst) n)))))))

;; (remove-xs '(a a b) '(a b a a c d e))
;; => (a c d e)
(defn remove-xs [xs lst]
  (if (or (empty? xs) (empty? lst))
    lst
    (if-let [found (find-x (first lst) xs)]
      (lazy-seq (remove-xs (remove-x found xs) (rest lst)))
      (cons (first lst) (lazy-seq (remove-xs xs (rest lst)))))))

;; (intersection '(a b c) '(b c d))
;; -> (b c)
(defn intersection [a b]
  (if (empty? a)
    nil
    (if (find-x (first a) b)
      (cons (first a) (intersection (rest a) b))
      (intersection (rest a) b))))

;; (set-difference '(a b c e) '(b c d)) ;=> (a e)
(defn set-difference [a b]
  (if (empty? a)
    nil
    (if (find-x (first a) b)
      (set-difference (rest a) b)
      (cons (first a) (set-difference (rest a) b)))))

(defn subset? [a b]
  "return a if a is subset of b"
  (if (every? #(find-x % b) a) a))

;; (remove-duplicate '((1 2 3) (4 5 6) 3 3 (1 2 3)))
;; => ((1 2 3) (4 5 6) 3)
;; (remove-duplicate (take 10 (iterate inc 0)) #(mod % 3))
;; => (0 1 2)
(defn remove-duplicate
  ([lst]
   (letfn [(rec [lst acc]
             (cond (empty? lst) (reverse acc)
                   (find-x (first lst) acc) (recur (rest lst) acc)
                   true (recur (rest lst) (cons (first lst) acc))))]
     (rec lst nil)))
  ([lst keyfn]
   (letfn [(rec [lst result tmp]
             (if (empty? lst) (reverse result)
                 (let [value (keyfn (first lst))]
                   (if (find-x value tmp)
                     (recur (rest lst) result tmp)
                     (recur (rest lst) (cons (first lst) result) (cons value tmp))))))]
     (rec lst nil nil))))

(defn find-duplicate
  ([lst]
   (letfn [(rec [[head & tail :as lst] result]
             (cond (empty? lst) (reverse result)
                   (find-x head tail) (recur (remove-x head tail) (cons head result))
                   true (recur tail result)))]
     (rec lst nil))))

(defn count-if [f [head & tail :as lst]]
  (if (empty? lst)
    0
    (if (f head)
      (inc (count-if f tail))
      (count-if f tail))))

;((juxt* inc (fn [x] nil) inc) 1)
;=>(2 2)
(defn juxt* [& [f & fns :as all]]
  (if (empty? all) (fn [x] nil)
    (fn [x] (let [val (f x) rest ((apply juxt* fns) x)]
              (if val (cons val rest) rest)))))

;; (or (f lst) (g lst) (h lst)) as
;; ((juxt-or f g h) lst)
(defn juxt-or [f & fns]
  (if (empty? fns)
    (fn [x] (f x))
    (fn [x] (or (f x) ((apply juxt-or fns) x)))))

;; (nanimati? "2334456789m中中中")
;; (concat (f lst) (g lst) (h lst)) as
;; ((juxt-cat f g h) lst)
;; sample
;; (let [animals (set '(dog cat))
;;       fruilt (set '(apple oarnge))]
;;   ((juxt-cat #(filter animals %) #(filter fruilt %))
;;    '(dog cat apple MahJong Lisp)))
;; => (dog cat apple)
(defn juxt-cat [& fns]
  (fn [& args] (apply concat (map #(apply % args) fns))))


;;(remove-nth 2 '(0 1 2 3 4))
;; => (0 1 3 4)
(defn remove-nth [n lst]
  (if (> n 0)
    (cons (first lst) (remove-nth (dec n) (rest lst)))
    (rest lst)))

;; (random-take 4 '(0 1 2 3))
;; => (2 0 1 3)
(defn random-take [n lst]
  (loop [n n lst lst acc nil]
    (if (<= n 0) acc
        (let [rnd (int (rand (count lst)))]
          (recur (dec n) (remove-nth rnd lst) (cons (nth lst rnd) acc))))))

(defn iter-perm [v]
  (let [len (count v)
        left (loop [i (- len 2)]
               (cond (< i 0) nil
                     (< (v i) (v (inc i))) i
                     :else (recur (dec i))))]
    (when left
      (let [vl (v left)
            right (loop [i (dec len)]
                    (if (< vl (v i)) i (recur (dec i))))]
        (loop [v (assoc v left (v right) right (v left)) left (inc left) right (dec len)]
          (if (< left right)
            (recur (assoc v left (v right) right (v left)) (inc left) (dec right))
            v))))))

; (map #(apply str %) (take 10 (permutations "Permtation of Ruby")))
;; ("Permtation of Ruby"
;;  "Permtation of Ruyb"
;;  "Permtation of Rbuy"
;;  "Permtation of Rbyu"
;;  "Permtation of Ryub"
;;  "Permtation of Rybu"
;;  "Permtation of uRby"
;;  "Permtation of uRyb"
;;  "Permtation of ubRy"
;;  "Permtation of ubyR")
(defn permutations [lst]
  (let [v (vec lst)]
    (map #(map v %) (iterate* iter-perm (vec (range (count lst)))))))

(defn sfirst [lst] (second (first lst)))

;; (($ str) (concat (map second (reverse buf)) (list (ffirst buf))))
;=> "2345m"
(defn $ [f] (fn [arg] (apply f arg)))

(defn count= [n seq] (= n (count seq)))

(defn c-to-i [c]
  (with-key int - c \0))

;; (s-to-i "12345") ;=> 12345
(defn s-to-i [s]
  (map-reduce c-to-i #(+ (* % 10) %2) s))

(defn =c [x] #(= x %))

(defn foldr [f z xs]
  (letfn [(step [g x a] (g (f x a)))]
    ((reduce (partial partial step) identity xs) z)))

(defn find-if [f [h & t :as xs]]
  (cond (empty? xs) nil
        (f h) h
        :else (recur f t)))

(defn flip [f] #(f %2 %1))

(defn include? [xs ys]
  (let [conv #(reduce (fn [map key] (ana-assoc map key (if it (inc it) 1))) {} %)
        xs (conv xs)
        ys (conv ys)]
    (if (empty? (keys xs))
      false
      (every? #(and (ys %) (<= (xs %) (ys %))) (keys xs)))))



