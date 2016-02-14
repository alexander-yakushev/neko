(ns org.example.coaflv.main
  (:require [clojure.string :as s]
            [neko.activity :refer [defactivity
                                   get-state
                                   set-content-view!]]
            [neko.debug :refer [*a]]
            [neko.find-view :refer [find-view]]
            [neko.threading :refer [on-ui]]
            [neko.ui.adapters :refer [ref-adapter]]
            )
  (:import android.text.TextWatcher
           android.widget.CheckBox
           android.widget.EditText
           ))

;; for better comprehension, look at the defactivity's onCreate first

;; to be displayed in list view
(def some-strings
  ["Ant" "Bee" "Cheetah" "Dog" "Elephant"
   "Fox" "Giraffe" "Human" "Iguana" "Jackal"
   "Kangaroo" "Llama" "Magpie" "Newt" "Ostrich"
   "Penguin" "Quail" "Rat" "Snake" "Tiger"
   "Uguisu" "Vulture" "Whale" "Xantus's hummingbird" ;; o rly?
   "Yak" "Zebra"])

(defn- make-list-view-pairs
  [a-vec]
  (mapv (fn [elt]
          [elt true])  ; true means display
        a-vec))

(def items-atom (atom (make-list-view-pairs some-strings)))

(defn- make-ref-adapter
  [a-ref]
  (ref-adapter (fn [c]
                 (CheckBox. c))
               (fn [_ v _ data]
                 (.setText v
                           (str data)))
               a-ref
               (fn [ref-content]
                 (keep (fn [[k v]]
                         (when v k))
                       ref-content))))

(defn- make-text-watcher
  [a-ref]
  (proxy [TextWatcher] []
    (afterTextChanged [_])
    (beforeTextChanged [_ _ _ _])
    (onTextChanged [a-str _ _ _]
      (let [lc-filter-str (s/lower-case (str a-str))]
        (swap! a-ref
               (fn [old]
                 ;; filter by substring match -- tweak this for other
                 ;; types of filtering
                 (mapv (fn [[name show]]
                         [name (< -1
                                  (.indexOf (s/lower-case name)
                                            lc-filter-str))])
                       old)))))))

(defactivity org.example.coaflv.MainActivity
  :key :main

  (onCreate [this bundle]
    (.superOnCreate this
                    bundle)
    (neko.debug/keep-screen-on this)    
    (let [;this (*a)
          ;; N.B. items-atom is top-level
          adapter (make-ref-adapter items-atom)
          text-watcher (make-text-watcher items-atom)]
      ;; so text-watcher can be retrieved later for proper clean-up
      (swap! (get-state this)
             assoc
             :text-watcher
             text-watcher)
      (on-ui
       (set-content-view! this
                          [:linear-layout {:layout-height :wrap
                                           :layout-width :fill
                                           :orientation :vertical}
                           [:edit-text {:hint "Type something here?"
                                        :id ::filter-text}]
                           [:list-view {:adapter adapter}]]))
      (.addTextChangedListener ^EditText (find-view this
                                                    ::filter-text)
                               text-watcher))
    )

  (onDestroy [this]
    (.superOnDestroy this)
    (.removeTextChangedListener (find-view this
                                           ::filter-text)
                                (:text-watcher @(get-state this))))
  )
