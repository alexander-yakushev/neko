(ns neko.ui.t-mapping
  (:require [clojure.test :refer :all]
            [neko.ui.mapping :refer :all :as m])
  (:import android.widget.TextView))

(deftest reverse-mapping
  (is (= (@@#'m/reverse-mapping android.widget.TextView) :text-view)))
