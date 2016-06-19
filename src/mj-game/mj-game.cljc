#?(:clj 
   (ns mj-game.mj-game
     (:require [ha2ne2.util :refer [atom? Y flatten1 empty-to-nil f s strict-take iterate*
                                    my-flatten map* repeat-cat =x with-key find-x remove-x
                                    remove-xs intersection set-difference subset? remove-duplicate
                                    find-duplicate count-if juxt* juxt-or juxt-cat remove-nth
                                    random-take iter-perm permutations sfirst $ count= c-to-i s-to-i
                                    =c find-if flip include? mapc]]
               [ha2ne2.mahjong-calc :as calc]
               [mj-game.problems :refer [problems]]))
   :cljs
   (ns mj-game.mj-game
     (:require [ha2ne2.util :refer [atom? Y flatten1 empty-to-nil f s strict-take iterate*
                                    my-flatten map* repeat-cat =x with-key find-x remove-x
                                    remove-xs intersection set-difference subset? remove-duplicate
                                    find-duplicate count-if juxt* juxt-or juxt-cat remove-nth
                                    random-take iter-perm permutations sfirst $ count= c-to-i s-to-i
                                    =c find-if flip include? mapc]]
               [ha2ne2.mahjong-calc :as calc]
               [mj-game.problems :refer [problems]]
               [goog.events :as events]
               [goog.dom :as dom])))

;; (def problems
;;   {
;;    "メヨセ" "0.25x1.2"
;;    "四分板" "0.25x8"
;;    "瓦桟" "0.5x1"
;;    "ラス板" "0.5x2"
;;    "ノゴメ" "0.5x1.5"
;;    "小桟" "0.9x1.2"
;;    "桟木" "0.9x2"
;;    "框"   "1.2角"
;;    "垂木" "1.5角"
;;    "根太" "1.5x2"
;;    "3寸角" "3角"
;;    }
;;   )

;; (def problems
;;   {
;;    "メヨセ" "8x36"
;;    "四分板" "8x240"
;;    "瓦桟"   "15x30"
;;    "ラス板" "15x60"
;;    "ノゴメ" "15x45"
;;    "小桟"   "27x36"
;;    "桟木"   "27x60"
;;    "框"     "36角"
;;    "垂木"   "45角"
;;    "根太"   "45x60"
;;    "3寸角"  "90角"
;;    }
;;   )



#?(:clj
   (defn game-start [problems]
     (loop [[p & ps] (random-take 5 problems)
            correct 0
            wrong   0]
       (if p
         (let [choices (shuffle [1 2 3 4])]
           (println p)
           (println (map-indexed #(str (inc %) "." %2) choices))
           (print "input your answer: ")
           (flush)
           (let [ans (nth choices (dec (read)))]
             (if (= "answer" ans)
               (do (println "CORRECT\n")
                   (recur ps (inc correct) wrong))
               (do (println "WRONG\n")
                   (recur ps correct (inc wrong))))))
         (println (str "YOUR SCORE IS " correct "/" 5))))))

;; 間違った問題をもう一度機能

#?(:clj 
   (defn -main
     "I don't do a whole lot ... yet."
     [& args]
     (println "Hello, World!"))
   :cljs
   (do
     (enable-console-print!)
     (def current-problems (atom nil))
     (def current-answer (atom nil))
     (def revenge-lst (atom []))
     (def revenge-mode (atom false))
     (def i (atom 0))
     (def correct-num (atom 0))
     (def wrong-num (atom 0))
     (def pbar (dom/getElement "pbar"))
     (def buffer1 (dom/getElement "buffer1"))
     (def btns [(dom/getElement "ans1")
                (dom/getElement "ans2")
                (dom/getElement "ans3")
                (dom/getElement "ans4")])
     (def start-btn (dom/getElement "start"))

     (defn show-result []
       (set! (.-innerHTML buffer1)
             (str "YOUR SCORE IS " @correct-num "/" 5))
       (mapc
        (fn [btn choice]
          (set! (.-innerHTML btn) "-")
          (set! (.-disabled btn) true))
        btns))

     (defn show-current-problem []
       (let [curr (nth @current-problems @i)
             choices (calc/choices-generator curr)]
         (reset! current-answer ((comp str :点 first) (calc/nan-ten? curr)))
         (set! (.-innerHTML buffer1) curr)
         (mapc
          (fn [btn choice]
            (set! (.-innerHTML btn) choice)
            (set! (.-disabled btn) false))
          btns
          choices)))

     (defn show-next-problem []
       (swap! i inc)
       (set! (.-value pbar) (int (* (/ @i (count @current-problems)) 100)))
       (if (= @i (count @current-problems))
         (do (reset! revenge-mode true)
             (if (empty? @revenge-lst)
               (show-result)
               (do (reset! current-problems @revenge-lst)
                   (reset! revenge-lst [])
                   (reset! i 0)
                   (set! (.-value pbar) 0)
                   (show-current-problem))))
         (show-current-problem)))
     
     (events/listen
      start-btn "click"
      (fn [event]
        (reset! current-problems
                (random-take 5 problems))
        (reset! correct-num 0)
        (reset! wrong-num 0)
        (reset! i 0)
        (reset! revenge-mode false)
        (set! (.-value pbar) 0)
        (show-current-problem)))
     
     (mapc
      (fn [btn]
        (events/listen
         btn "click"
         (fn [event]
           (println "button clicked" @current-answer)
           (if (= (.-innerHTML btn) @current-answer)
             (do
               (set! (-> buffer1 .-style .-backgroundColor) "#cccccc")
               (when (not @revenge-mode)
                 (swap! correct-num inc))
               (show-next-problem))
             (do
               (when (not @revenge-mode)
                 (swap! wrong-num inc)
                 (swap! correct-num dec)) ; その後+1されるので
               (set! (-> buffer1 .-style .-backgroundColor) "red")
               (swap! revenge-lst conj (nth @current-problems @i))
               (mapc #(set! (.-disabled %) true)
                     (filter #(not= (.-innerHTML %) @current-answer)
                             btns)))))))
      btns)

     (set! (.-disabled start-btn) false)
     ))


