; Copyright © 2011 Sattvik Software & Technology Resources, Ltd. Co.
; All rights reserved.
;
; This program and the accompanying materials are made available under the
; terms of the Eclipse Public License v1.0 which accompanies this distribution,
; and is available at <http://www.eclipse.org/legal/epl-v10.html>.
;
; By using this software in any fashion, you are agreeing to be bound by the
; terms of this license.  You must not remove this notice, or any other, from
; this software.

(ns neko.log
  "Utility for logging in Android. 

    (log-e \"Some log string\" x \"!\" :exception error)

  is equivalent to:

    (android.util.Log/d \"current.namespace\" (pr-str \"Some log string: \" x \"!\") error)

  Where the :exception is entirely optional."
  {:author "Daniel Solano Gómez"}
  (:require neko.context)
  (:import android.util.Log))

(defmacro deflogfn
  "Macro for generating log functions."
  {:private true}
  [fn-name doc-string method-name]
  `(defn ~fn-name
     ~doc-string
     ([^String tag#, ^String message#]
        (. Log (~method-name tag# message#))
        nil)
     ([^String tag#, ^String, message#, ^Throwable throwable#]
        (. Log (~method-name tag# message# throwable#))
        nil)))

(deflogfn d "Sends a DEBUG log message." d)
(deflogfn e "Sends a ERROR log message." e)
(deflogfn i "Sends a INFO log message." i)
(deflogfn v "Sends a VERBOSE log message." v)
(deflogfn w "Sends a WARN log message." w)

(defn- isnt-any-of [& args]
  (let [args (set args)]
      (fn [x] (not (contains? args x)))))

(defn- logger [logfn args]  
  (let [[strings {:keys [exception tag]}] (split-with (isnt-any-of :exception :tag) args)]
    (if exception
      (list logfn
            (list 'clojure.core/or tag (list 'clojure.core/str 'clojure.core/*ns*))
            (concat (list 'clojure.core/pr-str) strings)
            exception)
      (list logfn
            (list 'clojure.core/or tag (list 'clojure.core/str 'clojure.core/*ns*))
            (concat (list 'clojure.core/pr-str) strings)))))

(defmacro log-e
  "Log an ERROR message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger e args))

(defmacro log-d
  "Log a DEBUG message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger d args))

(defmacro log-i
  "Log an INFO message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger i args))

(defmacro log-v
  "Log a VERBOSE message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger v args))

(defmacro log-w
  "Log a WARN message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger w args))


;;; DEPRECATED FUNCTIONALITY RETAINED FOR BACK COMPATIBILITY:

;;; Functionality can be reproduced with other logging functions:
(defn log-exception
  "Takes a Throwable instance and logs its stacktrace with error priority."
  [throwable]
  (e (.getPackageName neko.context/context)
     (android.util.Log/getStackTraceString throwable)))

;;; Candidate for complete removal, pollutes namespaces and encourages
;;; inconsistent tags
(defmacro deflog
  "DEPRECATED - do not use! Creates a number of logging functions for the given tag."
  {:deprecated "3.0.0"}
  [tag]  
  (let [intern-logger
        (fn [log-name log-fn]
          `(intern *ns* (symbol ~log-name)
                   (with-meta (partial ~log-fn ~tag) {:private true})))]
    `(do
       ~(intern-logger "log-d" `d)
       ~(intern-logger "log-e" `e)
       ~(intern-logger "log-i" `i)
       ~(intern-logger "log-v" `v)
       ~(intern-logger "log-w" `w))))
