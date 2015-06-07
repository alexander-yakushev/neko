(defproject neko "4.0.0-SNAPSHOT"
  :description "Neko is a toolkit designed to make Android development using Clojure easier and more fun."
  :url "https://github.com/clojure-android/neko"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure-android/clojure "1.7.0-alpha6"]]
  :source-paths ["src" "src/clojure"]
  :java-source-paths ["src/java"]

  :profiles {:default [:android-common]}
  :plugins [[lein-droid "0.3.0"]]

  :android {:library true
            :target-version 18})
