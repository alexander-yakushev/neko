(defproject neko "2.0.0-beta2"
  :description "Neko is a toolkit designed to make Android development using Clojure easier and more fun."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure-android/clojure "1.5.1"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]

  :android {:library true
            :target-sdk "17"})
