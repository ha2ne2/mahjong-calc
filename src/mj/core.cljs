(ns mj.core
  (:require [ha2ne2.mahjong-calc :as calc]
            [goog.events :as events]
            [goog.dom :as dom]))

(defn main
  []
  (let [counter (atom 0)
        button  (dom/getElement "button")
        display (dom/getElement "clicksnumber")
        hand    (dom/getElement "hand")
        calc-btn    (dom/getElement "calc")
        clear-btn   (dom/getElement "clear")
        result-area (dom/getElement "result")
        ]

    ;; Set initial value
    (set! (.-innerHTML display) @counter)

    ;; Assign event listener
    (events/listen button "click"
                   (fn [event]
                     ;; Increment the value
                     (swap! counter inc)
                     ;; Set new value in display element
                     (set! (.-innerHTML display) @counter)))

    (events/listen calc-btn "click"
                   (fn [event]
                     (set! (.-innerHTML result-area) (calc/nan-ten?-with-img (.-value hand)))
                     ))

    (events/listen clear-btn "click"
                   (fn [event]
                     (set! (.-value hand) "")
                     ))))


(main)

(enable-console-print!)
(println "Hello, World!")
